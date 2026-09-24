# These test helpers mirror the ones at the top of test-write.R: testthat
# runs each file in its own environment, so they are repeated here.

# The signed-in account is "42" for every test here, with no users/me call.
use_me <- function(env = parent.frame()) {
  .x_env$my_user_id <- list(key = "tok", id = "42")
  withr::defer(.x_env$my_user_id <- NULL, envir = env)
}

api_user <- function(i) {
  list(id = as.character(i), username = paste0("u", i), name = paste("User", i))
}

test_that("search_users pages with next_token and stops at max_users", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    if (grepl("next_token=abc", req$url)) {
      json_response(200, list(
        data = list(api_user(3), api_user(4)),
        meta = list(next_token = "def")
      ))
    } else {
      json_response(200, list(
        data = list(api_user(1), api_user(2)),
        meta = list(next_token = "abc")
      ))
    }
  })

  msgs <- character(0)
  users <- withCallingHandlers(
    search_users("electric", max_results = 2, max_users = 3),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_s3_class(users, "tbl_df")
  expect_equal(names(users), names(user_schema()))
  expect_equal(users$user_id, c("1", "2", "3"))

  urls <- vapply(seen(), function(r) r$url, "")
  expect_equal(length(urls), 2)
  expect_match(urls[1], "^https://api.x.com/2/users/search\\?")
  expect_match(urls[1], "query=electric", fixed = TRUE)
  expect_match(urls[1], "max_results=2", fixed = TRUE)
  expect_match(urls[1], "user.fields=", fixed = TRUE)
  expect_false(grepl("next_token", urls[1]))
  expect_match(urls[2], "next_token=abc", fixed = TRUE)
  expect_match(urls[2], "max_results=1", fixed = TRUE)
  expect_false(any(grepl("pagination_token", urls)))
  expect_equal(seen()[[1]]$headers$Authorization, "Bearer tok")

  expect_match(msgs[1], "Reading up to 3 users, about \\$0.03. Set max_users")
  expect_match(msgs[2], "page 1")
  expect_match(msgs[3], "page 2")
  expect_match(msgs[4], "Read 3 users, about \\$0.03")
})

test_that("search_users stops when the API has no next page", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(api_user(1)), meta = list()))
  })
  users <- suppressMessages(search_users("electric", max_users = 50))
  expect_equal(nrow(users), 1)
  expect_equal(length(seen()), 1)
  expect_match(seen()[[1]]$url, "max_results=50", fixed = TRUE)
})

test_that("search_users with no match is the zero-row schema", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    json_response(200, list(meta = list(result_count = 0L)))
  })
  expect_identical(suppressMessages(search_users("zzz")), user_schema())
})

test_that("search_users checks its arguments before any request", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  expect_error(search_users(""), "`query` must be one search string")
  expect_error(search_users("a", max_results = 0), "between 1 and 1000")
  expect_error(search_users("a", max_results = 1001), "between 1 and 1000")
  expect_error(search_users("a", max_users = Inf), "finite number")
})

test_that("get_personalized_trends reads the four trend fields", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(
      list(trend_name = "#EVs", category = "Technology", post_count = "1234",
           trending_since = "3 hours ago"),
      list(trend_name = "Robotaxi", category = "Business", post_count = "12.5K")
    )))
  })

  expect_message(trends <- get_personalized_trends(),
                 "This request costs about \\$0.010")

  expect_s3_class(trends, "tbl_df")
  expect_equal(names(trends),
               c("trend_name", "category", "post_count", "trending_since"))
  expect_equal(trends$trend_name, c("#EVs", "Robotaxi"))
  expect_equal(trends$post_count, c(1234L, NA_integer_))
  expect_equal(trends$trending_since, c("3 hours ago", NA_character_))

  req <- seen()[[1]]
  expect_null(req$method)  # httr2 leaves a GET unset
  expect_match(req$url, "^https://api.x.com/2/users/personalized_trends\\?")
  expect_match(req$url, "personalized_trend.fields=category%2Cpost_count%2Ctrend_name%2Ctrending_since",
               fixed = TRUE)
})

test_that("get_personalized_trends with no trends is the zero-row schema", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    json_response(200, list(data = list()))
  })
  trends <- suppressMessages(get_personalized_trends())
  expect_equal(nrow(trends), 0)
  expect_equal(names(trends),
               c("trend_name", "category", "post_count", "trending_since"))
})

test_that("get_personalized_trends stops with the API's detail on a 403", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    json_response(403, list(
      title = "Forbidden", detail = "Your tier cannot read trends.", status = 403
    ))
  })
  expect_error(suppressMessages(get_personalized_trends()), "cannot read trends")
})

bucket <- function(ts, impressions, retweets, quote_tweets, email_tweet) {
  list(timestamp = ts, metrics = list(
    impressions = impressions, engagements = 1L, likes = 2L,
    retweets = retweets, replies = 0L, quote_tweets = quote_tweets,
    bookmarks = 0L, follows = 0L, unfollows = 0L, url_clicks = 0L,
    user_profile_clicks = 0L, media_views = 0L, detail_expands = 0L,
    permalink_clicks = 0L, hashtag_clicks = 0L, shares = 0L, app_opens = 0L,
    app_install_attempts = 0L, email_tweet = email_tweet, unlikes = 0L
  ))
}

analytics_columns <- c(
  "post_id", "timestamp", "impressions", "engagements", "likes", "reposts",
  "replies", "quotes", "bookmarks", "follows", "unfollows", "url_clicks",
  "user_profile_clicks", "media_views", "detail_expands", "permalink_clicks",
  "hashtag_clicks", "shares", "app_opens", "app_install_attempts",
  "email_post", "unlikes"
)

test_that("get_post_analytics flattens timestamped metrics, one row per period", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(
      list(
        id = "1",
        timestamped_metrics = list(
          bucket("2026-09-01T00:00:00Z", 100L, 5L, 2L, 1L),
          bucket("2026-09-02T00:00:00Z", 50L, 1L, 0L, 0L)
        )
      ),
      list(
        id = "2",
        timestamped_metrics = list(
          bucket("2026-09-01T00:00:00Z", 7L, 0L, 0L, 0L)
        )
      )
    )))
  })

  expect_no_message(
    out <- get_post_analytics(
      c("1", "2"), "2026-09-01T00:00:00Z", "2026-09-03T00:00:00Z",
      granularity = "daily"
    )
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), analytics_columns)
  expect_equal(nrow(out), 3)
  expect_equal(out$post_id, c("1", "1", "2"))
  expect_s3_class(out$timestamp, "POSIXct")
  expect_equal(attr(out$timestamp, "tzone"), "UTC")
  expect_equal(
    out$timestamp,
    lubridate::ymd_hms(c("2026-09-01 00:00:00", "2026-09-02 00:00:00",
                         "2026-09-01 00:00:00"), tz = "UTC")
  )
  expect_equal(out$impressions, c(100L, 50L, 7L))
  expect_equal(out$reposts, c(5L, 1L, 0L))
  expect_equal(out$quotes, c(2L, 0L, 0L))
  expect_equal(out$email_post, c(1L, 0L, 0L))
  expect_true(all(vapply(out[-(1:2)], is.integer, TRUE)))
  expect_false(any(c("retweets", "quote_tweets", "email_tweet") %in% names(out)))

  req <- seen()[[1]]
  expect_equal(length(seen()), 1)
  expect_null(req$method)  # httr2 leaves a GET unset
  expect_match(req$url, "^https://api.x.com/2/tweets/analytics\\?")
  expect_match(req$url, "ids=1%2C2", fixed = TRUE)
  expect_match(req$url, "start_time=2026-09-01T00%3A00%3A00Z", fixed = TRUE)
  expect_match(req$url, "end_time=2026-09-03T00%3A00%3A00Z", fixed = TRUE)
  expect_match(req$url, "granularity=daily", fixed = TRUE)
  expect_match(req$url, "analytics.fields=id%2Ctimestamp%2Ctimestamped_metrics%2Cimpressions",
               fixed = TRUE)
  expect_equal(req$headers$Authorization, "Bearer tok")
})

test_that("get_post_analytics reads top-level metrics for a total, with NA timestamp", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    json_response(200, list(data = list(
      list(id = "1", impressions = 300L, retweets = 4L, quote_tweets = 1L,
           email_tweet = 0L, likes = 9L)
    )))
  })
  out <- get_post_analytics("1", as.Date("2026-09-01"), as.Date("2026-09-08"))
  expect_equal(nrow(out), 1)
  expect_equal(names(out), analytics_columns)
  expect_true(is.na(out$timestamp))
  expect_equal(out$impressions, 300L)
  expect_equal(out$reposts, 4L)
  expect_equal(out$quotes, 1L)
  expect_equal(out$likes, 9L)
  expect_true(is.na(out$shares))
})

test_that("get_post_analytics with no data is the zero-row schema", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    json_response(200, list(data = list()))
  })
  out <- get_post_analytics("1", "2026-09-01T00:00:00Z", "2026-09-08T00:00:00Z")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), analytics_columns)
  expect_s3_class(out$timestamp, "POSIXct")
})

test_that("get_post_analytics checks its arguments and passes a 403 on", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  expect_error(get_post_analytics(1, "a", "b"), "strings of digits")
  expect_error(get_post_analytics(as.character(1:101), "a", "b"), "at most 100")
  expect_error(get_post_analytics("1", "", "b"), "`start_time` must be")
  expect_error(get_post_analytics("1", "a", "b", granularity = "monthly"),
               "\"hourly\", \"daily\", \"weekly\" or \"total\"")

  httr2::local_mocked_responses(function(req) {
    json_response(403, list(
      title = "Forbidden", detail = "You do not own post 1.", status = 403
    ))
  })
  expect_error(get_post_analytics("1", "2026-09-01T00:00:00Z", "2026-09-08T00:00:00Z"),
               "do not own post 1")
})
