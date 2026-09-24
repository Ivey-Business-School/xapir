# A user object as the API sends it, with the fields the package reads.
api_user <- function(id, username = paste0("user", id), url = "https://t.co/x") {
  list(
    id = as.character(id),
    username = username,
    name = paste("User", id),
    description = "bio",
    created_at = "2020-01-02T03:04:05.000Z",
    protected = FALSE,
    verified = TRUE,
    verified_type = "blue",
    is_identity_verified = FALSE,
    location = "Toronto",
    profile_image_url = "https://pbs.twimg.com/x.jpg",
    profile_banner_url = "https://pbs.twimg.com/banner.jpg",
    verified_followers_count = 3L,
    subscription_type = "Premium",
    parody = FALSE,
    pinned_tweet_id = "555",
    url = url,
    entities = list(url = list(urls = list(list(display_url = "example.com")))),
    public_metrics = list(
      followers_count = 10L, following_count = 5L, tweet_count = 100L,
      listed_count = 2L, like_count = 7L, media_count = 40L
    )
  )
}

# A page of users with the given ids, and a next_token when given.
users_page <- function(ids, next_token = NULL) {
  meta <- list(result_count = length(ids))
  if (!is.null(next_token)) meta$next_token <- next_token
  json_response(200, list(data = lapply(ids, api_user), meta = meta))
}

user_columns <- c(
  "created_at", "username", "name", "description", "followers_count",
  "following_count", "post_count", "listed_count", "like_count",
  "media_count", "protected", "verified", "verified_type",
  "verified_followers_count", "subscription_type", "parody",
  "is_identity_verified", "location", "profile_image_url",
  "profile_banner_url", "link_in_bio", "url", "pinned_post_id", "user_id"
)

mock_user_token <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    authenticate_user = function(...) list(access_token = "tok"),
    .package = "xapir",
    .env = env
  )
}

test_that("user_row fills every column with a typed NA from an empty list", {
  row <- user_row(list())
  expect_equal(names(row), user_columns)
  expect_equal(nrow(row), 1)
  expect_true(all(vapply(row, function(col) is.na(col), TRUE)))
  expect_s3_class(row$created_at, "POSIXct")
  expect_equal(attr(row$created_at, "tzone"), "UTC")
  expect_type(row$username, "character")
  expect_type(row$followers_count, "integer")
  expect_type(row$post_count, "integer")
  expect_type(row$media_count, "integer")
  expect_type(row$protected, "logical")
  expect_type(row$verified_followers_count, "integer")
  expect_type(row$subscription_type, "character")
  expect_type(row$parody, "logical")
  expect_type(row$is_identity_verified, "logical")
  expect_type(row$profile_banner_url, "character")
  expect_type(row$url, "character")
  expect_type(row$pinned_post_id, "character")
  expect_type(row$user_id, "character")
})

test_that("user_row reads a full user, turns an empty url into NA", {
  row <- user_row(api_user(1))
  expect_equal(row$user_id, "1")
  expect_equal(row$post_count, 100L)
  expect_equal(row$media_count, 40L)
  expect_equal(row$verified_followers_count, 3L)
  expect_equal(row$subscription_type, "Premium")
  expect_false(row$parody)
  expect_equal(row$profile_banner_url, "https://pbs.twimg.com/banner.jpg")
  expect_equal(row$link_in_bio, "example.com")
  expect_equal(row$url, "https://t.co/x")
  expect_equal(row$pinned_post_id, "555")
  expect_equal(format(row$created_at, "%Y-%m-%d %H:%M:%S"), "2020-01-02 03:04:05")
  expect_true(is.na(user_row(api_user(2, url = ""))$url))
})

test_that("user_row leaves the newer fields NA when the API omits them", {
  x <- api_user(3)
  x$profile_banner_url <- NULL
  x$verified_followers_count <- NULL
  x$subscription_type <- NULL
  x$parody <- NULL
  x$pinned_tweet_id <- NULL
  x$public_metrics$media_count <- NULL
  row <- user_row(x)
  expect_equal(names(row), user_columns)
  expect_true(is.na(row$media_count))
  expect_true(is.na(row$verified_followers_count))
  expect_true(is.na(row$subscription_type))
  expect_true(is.na(row$parody))
  expect_true(is.na(row$profile_banner_url))
  expect_true(is.na(row$pinned_post_id))
  expect_equal(row$user_id, "3")
})

test_that("users_table on nothing is the zero-row schema, silently", {
  expect_silent(empty <- users_table(list()))
  expect_identical(empty, user_schema())
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), user_columns)
  expect_identical(users_table(NULL), user_schema())
})

test_that("users_table keeps one row per user id and binds to the schema", {
  users <- users_table(list(api_user(1), api_user(2), api_user(1)))
  expect_equal(nrow(users), 2)
  expect_equal(users$user_id, c("1", "2"))
  expect_identical(
    vapply(users, function(col) class(col)[1], ""),
    vapply(user_schema(), function(col) class(col)[1], "")
  )
})

test_that("extract_user still gives the typed, filled table the package promises", {
  user <- extract_user(tesla_pages())
  expect_equal(names(user), user_columns)
  expect_equal(nrow(user), length(unique(user$user_id)))
  expect_type(user$is_identity_verified, "logical")
  expect_false(any(is.na(user$is_identity_verified)))
  expect_type(user$url, "character")
  expect_gt(sum(!is.na(user$url)), 0)
  expect_equal(attr(user$created_at, "tzone"), "UTC")
  always_filled <- c("created_at", "username", "name", "description",
                     "followers_count", "following_count", "post_count",
                     "listed_count", "like_count", "protected", "verified",
                     "verified_type", "profile_image_url", "user_id")
  for (col in always_filled) {
    expect_false(any(is.na(user[[col]])), info = col)
  }
  # the fixture was pulled before the newer fields joined the defaults, but
  # media_count and pinned_tweet_id already came back
  expect_type(user$media_count, "integer")
  expect_false(any(is.na(user$media_count)))
  expect_type(user$pinned_post_id, "character")
  expect_gt(sum(!is.na(user$pinned_post_id)), 0)
  expect_type(user$verified_followers_count, "integer")
  expect_type(user$subscription_type, "character")
  expect_type(user$parody, "logical")
  expect_type(user$profile_banner_url, "character")
})

test_that("extract_user on an empty timeline is the zero-row schema, silently", {
  expect_silent(expect_identical(extract_user(list()), user_schema()))
  expect_silent(expect_identical(extract_user(NULL), user_schema()))
  expect_identical(extract_user(list(list(data = list()))), user_schema())
})

test_that("get_users_by_usernames warns once about a bad handle and keeps the good rows", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(
      data = list(api_user(1, username = "tesla")),
      errors = list(list(
        title = "Not Found Error",
        detail = "Could not find user with usernames: [nobodyhere]."
      ))
    ))
  })
  msgs <- character(0)
  warns <- character(0)
  users <- withCallingHandlers(
    get_users_by_usernames(c("@tesla", "nobodyhere"), bearer_token = "tok"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(length(warns), 1)
  expect_match(warns, "nobodyhere")
  expect_equal(nrow(users), 1)
  expect_equal(users$username, "tesla")
  expect_equal(names(users), user_columns)
  expect_match(msgs[1], "Reading up to 2 users, about \\$0.02")
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/users/by?", fixed = TRUE)
  expect_match(urls[1], "usernames=tesla%2Cnobodyhere", fixed = TRUE)
})

test_that("get_users_by_usernames with no handle found is the zero-row schema", {
  httr2::local_mocked_responses(list(
    json_response(200, list(errors = list(list(
      title = "Not Found Error",
      detail = "Could not find user with usernames: [nobodyhere]."
    ))))
  ))
  expect_warning(
    users <- suppressMessages(get_users_by_usernames("nobodyhere", bearer_token = "tok")),
    "Could not find user"
  )
  expect_identical(users, user_schema())
})

test_that("get_users_by_ids reads data only and honours the price option", {
  httr2::local_mocked_responses(list(
    json_response(200, list(data = list(api_user(1), api_user(2))))
  ))
  old <- options(xapir.price_per_user = 0.5)
  on.exit(options(old), add = TRUE)
  expect_message(
    users <- get_users_by_ids(c("1", "2"), bearer_token = "tok"),
    "Reading up to 2 users, about \\$1.00"
  )
  expect_equal(users$user_id, c("1", "2"))
  expect_equal(names(users), user_columns)
})

test_that("more than 100 ids or handles stops before any request", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(
    get_users_by_ids(as.character(1:101), bearer_token = "tok"),
    "at most 100"
  )
  expect_error(
    get_users_by_usernames(paste0("u", 1:101), bearer_token = "tok"),
    "at most 100"
  )
})

test_that("ids must be text, and a missing token stops first", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_users_by_ids(c(1, 2), bearer_token = "tok"), "as text")
  expect_error(get_users_by_ids(character(0), bearer_token = "tok"), "one or more")
  expect_error(get_users_by_usernames(character(0), bearer_token = "tok"), "one or more")
  expect_error(get_users_by_ids("1", bearer_token = ""), "X_BEARER_TOKEN")
  expect_error(get_users_by_usernames("tesla", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("get_my_user returns one row from the single object in data", {
  mock_user_token()
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(data = api_user(42, username = "me")))
  })
  me <- get_my_user()
  expect_equal(nrow(me), 1)
  expect_equal(me$user_id, "42")
  expect_equal(names(me), user_columns)
  expect_match(urls[1], "/2/users/me", fixed = TRUE)
})

test_that("get_blocking pages twice and returns every user", {
  mock_user_token()
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("/users/me", req$url, fixed = TRUE)) {
      json_response(200, list(data = list(id = "42", username = "me")))
    } else if (grepl("pagination_token=n2", req$url, fixed = TRUE)) {
      users_page(3:4)
    } else {
      users_page(1:2, next_token = "n2")
    }
  })
  msgs <- character(0)
  blocked <- withCallingHandlers(
    get_blocking(),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  # a block read is $0.001 a user, not the $0.010 of a profile read
  expect_match(msgs[1], "Reading up to 500 users, about \\$0.50")
  expect_equal(length(urls), 3)
  expect_match(urls[2], "/2/users/42/blocking", fixed = TRUE)
  expect_match(urls[3], "/2/users/42/blocking", fixed = TRUE)
  expect_equal(nrow(blocked), 4)
  expect_equal(blocked$user_id, c("1", "2", "3", "4"))
  expect_equal(names(blocked), user_columns)
})

test_that("get_blocking and get_muting stop at max_users and validate max_results", {
  mock_user_token()
  httr2::local_mocked_responses(function(req) {
    if (grepl("/users/me", req$url, fixed = TRUE)) {
      json_response(200, list(data = list(id = "42")))
    } else {
      users_page(1:20, next_token = "more")
    }
  })
  blocked <- suppressMessages(get_blocking(max_users = 15))
  expect_equal(nrow(blocked), 15)
  muted <- suppressMessages(get_muting(max_users = 15))
  expect_equal(nrow(muted), 15)

  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_blocking(max_results = 1000), "between 10 and 100")
  expect_error(get_muting(max_results = 5), "between 10 and 100")
  expect_error(get_muting(max_users = 0), "1 or more")
})

test_that("get_muting with nobody muted is the zero-row schema", {
  mock_user_token()
  httr2::local_mocked_responses(function(req) {
    if (grepl("/users/me", req$url, fixed = TRUE)) {
      json_response(200, list(data = list(id = "42")))
    } else {
      json_response(200, list(meta = list(result_count = 0L)))
    }
  })
  muted <- suppressMessages(get_muting())
  expect_identical(muted, user_schema())
})
