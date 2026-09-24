# Every test here mocks the API. Nothing calls X.

collect_messages <- function(expr) {
  msgs <- character(0)
  result <- withCallingHandlers(
    expr,
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(result = result, msgs = trimws(msgs))
}

# A page of archive counts, with a next_token when given.
all_count_page <- function(rows, next_token = NULL) {
  meta <- list(total_post_count = sum(vapply(rows, function(r) r$post_count, 1L)))
  if (!is.null(next_token)) meta$next_token <- next_token
  json_response(200, list(data = rows, meta = meta))
}

count_row <- function(day, n) {
  list(
    start = sprintf("2020-02-%02dT00:00:00.000Z", day),
    end = sprintf("2020-02-%02dT00:00:00.000Z", day + 1),
    post_count = as.integer(n)
  )
}

# Full-archive search --------------------------------------------------------

test_that("get_all_post hits /search/all with the query and returns pages", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("pagination_token=n2", req$url, fixed = TRUE)) {
      posts_page(101:120)
    } else {
      posts_page(1:100, next_token = "n2")
    }
  })
  out <- collect_messages(get_all_post(
    "#SuperBowl lang:en",
    start_time = "2020-02-01T00:00:00Z",
    end_time = "2020-02-03T00:00:00Z",
    sort_order = "recency",
    bearer_token = "tok"
  ))
  expect_match(out$msgs[1], "Reading up to 500 posts, about \\$2\\.50")
  expect_equal(length(urls), 2)
  expect_match(urls[1], "/2/tweets/search/all?", fixed = TRUE)
  expect_match(urls[1], "query=%23SuperBowl%20lang%3Aen", fixed = TRUE)
  expect_match(urls[1], "start_time=2020-02-01T00%3A00%3A00Z", fixed = TRUE)
  expect_match(urls[1], "sort_order=recency", fixed = TRUE)
  expect_match(urls[1], "tweet.fields=created_at", fixed = TRUE)
  expect_match(urls[1], "max_results=100", fixed = TRUE)
  expect_match(urls[2], "pagination_token=n2", fixed = TRUE)
  pages <- out$result
  expect_equal(length(pages), 2)
  expect_equal(sum(vapply(pages, function(p) length(p$data), 1L)), 120)
  expect_equal(nrow(extract_post(pages)), 120)
  expect_equal(out$msgs[length(out$msgs)], "Read 120 posts, about $0.60.")
})

test_that("get_all_post never exceeds max_posts, whatever the API sends", {
  httr2::local_mocked_responses(function(req) posts_page(1:500, next_token = "more"))
  pages <- suppressMessages(get_all_post(
    "tesla", max_results = 500, max_posts = 750, bearer_token = "tok"
  ))
  expect_equal(length(pages), 2)
  expect_equal(sum(vapply(pages, function(p) length(p$data), 1L)), 750)

  pages <- suppressMessages(get_all_post("tesla", max_posts = 30, bearer_token = "tok"))
  expect_equal(sum(vapply(pages, function(p) length(p$data), 1L)), 30)
})

test_that("get_all_post takes 10 to 500 a page and validates before any request", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_all_post("tesla", max_results = 501, bearer_token = "tok"), "between 10 and 500")
  expect_error(get_all_post("tesla", max_results = 9, bearer_token = "tok"), "between 10 and 500")
  expect_error(get_all_post("tesla", max_posts = Inf, bearer_token = "tok"), "finite number")
  expect_error(get_all_post("", bearer_token = "tok"), "`query` must be one search string")
  expect_error(get_all_post("tesla", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("get_all_post with nothing found gives one page with no data", {
  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  pages <- suppressMessages(get_all_post("nothingmatchesthis", bearer_token = "tok"))
  expect_equal(length(pages), 1)
  expect_equal(length(pages[[1]]$data), 0)
  expect_equal(nrow(extract_post(pages)), 0)
})

# Full-archive counts --------------------------------------------------------

test_that("get_all_post_count follows next_token, binds the rows and drops the edges", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("next_token=c2", req$url, fixed = TRUE)) {
      all_count_page(list(count_row(4, 40), count_row(5, 5)))
    } else {
      all_count_page(list(count_row(1, 1), count_row(2, 20), count_row(3, 30)), next_token = "c2")
    }
  })
  out <- collect_messages(get_all_post_count(
    "tesla", start_time = "2020-02-01T12:00:00Z", end_time = "2020-02-05T12:00:00Z",
    is_local_tz = FALSE, bearer_token = "tok"
  ))
  # a count is $0.010 a request, and two requests were needed
  expect_match(out$msgs[1], "This request costs about \\$0\\.010")
  expect_equal(out$msgs[2], "2 requests, about $0.02 in total.")
  expect_equal(length(urls), 2)
  expect_match(urls[1], "/2/tweets/counts/all?", fixed = TRUE)
  expect_match(urls[1], "granularity=day", fixed = TRUE)
  expect_match(urls[1], "query=tesla", fixed = TRUE)
  expect_false(grepl("next_token", urls[1], fixed = TRUE))
  expect_match(urls[2], "next_token=c2", fixed = TRUE)
  counts <- out$result
  expect_s3_class(counts, "tbl_df")
  expect_equal(names(counts), c("start", "end", "post_count"))
  expect_equal(counts$post_count, c(20L, 30L, 40L))
  expect_s3_class(counts$start, "POSIXct")
  expect_equal(format(counts$start[1], "%Y-%m-%d", tz = "UTC"), "2020-02-02")
})

test_that("get_all_post_count keeps every row when drop_incomplete is FALSE and reads tweet_count too", {
  httr2::local_mocked_responses(list(json_response(200, list(
    data = list(
      list(start = "2020-02-01T00:00:00.000Z", end = "2020-02-02T00:00:00.000Z", tweet_count = 2L),
      list(start = "2020-02-02T00:00:00.000Z", end = "2020-02-03T00:00:00.000Z", tweet_count = 5L)
    ),
    meta = list(total_tweet_count = 7L)
  ))))
  out <- collect_messages(get_all_post_count(
    "tesla", drop_incomplete = FALSE, is_local_tz = FALSE, bearer_token = "tok"
  ))
  expect_equal(out$result$post_count, c(2L, 5L))
  # one page, so only the one cost line
  expect_equal(length(out$msgs), 1)
})

test_that("get_all_post_count returns the empty schema when there is no data", {
  httr2::local_mocked_responses(list(
    json_response(200, list(data = list(), meta = list(total_post_count = 0L))),
    json_response(200, list(meta = list(total_post_count = 0L)))
  ))
  for (i in 1:2) {
    counts <- suppressMessages(get_all_post_count("nothingmatchesthis", bearer_token = "tok"))
    expect_s3_class(counts, "tbl_df")
    expect_equal(nrow(counts), 0)
    expect_equal(names(counts), c("start", "end", "post_count"))
    expect_s3_class(counts$start, "POSIXct")
    expect_type(counts$post_count, "integer")
  }
})

test_that("get_all_post_count stops before a request on bad input", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_all_post_count("", bearer_token = "tok"), "`query` must be one search string")
  expect_error(get_all_post_count("tesla", granularity = "week", bearer_token = "tok"), "\"minute\", \"hour\" or \"day\"")
  expect_error(get_all_post_count("tesla", bearer_token = ""), "X_BEARER_TOKEN")
})

# Usage ----------------------------------------------------------------------

test_that("get_usage returns the one-row summary with the daily rows attached, silently", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(data = list(
      project_id = "1234",
      project_cap = "10000",
      project_usage = "250",
      cap_reset_day = 15L,
      daily_project_usage = list(
        project_id = "1234",
        usage = list(
          list(date = "2026-09-23T00:00:00.000Z", usage = "200"),
          list(date = "2026-09-22T00:00:00.000Z", usage = "50")
        )
      ),
      daily_client_app_usage = list()
    )))
  })
  expect_silent(usage <- get_usage(days = 2, bearer_token = "tok"))
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/usage/tweets?", fixed = TRUE)
  expect_match(urls[1], "days=2", fixed = TRUE)
  expect_match(urls[1], "usage.fields=cap_reset_day%2Cdaily_client_app_usage%2Cdaily_project_usage%2Cproject_cap%2Cproject_id%2Cproject_usage", fixed = TRUE)
  expect_s3_class(usage, "tbl_df")
  expect_equal(nrow(usage), 1)
  expect_equal(names(usage), c("project_id", "project_cap", "project_usage", "cap_reset_day"))
  expect_equal(usage$project_id, "1234")
  expect_identical(usage$project_cap, 10000L)
  expect_identical(usage$project_usage, 250L)
  expect_identical(usage$cap_reset_day, 15L)
  daily <- attr(usage, "daily")
  expect_s3_class(daily, "tbl_df")
  expect_equal(names(daily), c("date", "usage"))
  expect_s3_class(daily$date, "Date")
  expect_equal(as.character(daily$date), c("2026-09-22", "2026-09-23"))
  expect_identical(daily$usage, c(50L, 200L))
})

test_that("get_usage with no data gives typed NAs and an empty daily table, and validates days", {
  httr2::local_mocked_responses(list(json_response(200, list(data = list()))))
  usage <- get_usage(bearer_token = "tok")
  expect_equal(nrow(usage), 1)
  expect_true(is.na(usage$project_cap))
  expect_type(usage$project_cap, "integer")
  expect_type(usage$project_id, "character")
  expect_equal(nrow(attr(usage, "daily")), 0)

  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_usage(days = 0, bearer_token = "tok"), "between 1 and 90")
  expect_error(get_usage(days = 91, bearer_token = "tok"), "between 1 and 90")
  expect_error(get_usage(bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("get_usage_credits returns the three balances as doubles, silently", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(data = list(
      total_balance = 12.5, prepaid_balance = 10, free_balance = 2.5,
      free_grants = list()
    )))
  })
  expect_silent(credits <- get_usage_credits(bearer_token = "tok"))
  expect_match(urls[1], "/2/usage/credits", fixed = TRUE)
  expect_equal(nrow(credits), 1)
  expect_equal(names(credits), c("total_balance", "prepaid_balance", "free_balance"))
  expect_type(credits$total_balance, "double")
  expect_equal(credits$total_balance, 12.5)
  expect_equal(credits$prepaid_balance, 10)
  expect_equal(credits$free_balance, 2.5)
  expect_error(get_usage_credits(bearer_token = ""), "X_BEARER_TOKEN")
})
