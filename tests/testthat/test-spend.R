# get_spend --------------------------------------------------------------------

usage_response <- function(entries) {
  json_response(200, list(data = list(
    project_id = "1234",
    project_cap = "10000",
    project_usage = "250",
    cap_reset_day = 15L,
    daily_project_usage = list(project_id = "1234", usage = entries),
    daily_client_app_usage = list()
  )))
}

test_that("get_spend prices each day's post reads, oldest first, and prints one summary line", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    usage_response(list(
      list(date = "2026-09-23T00:00:00.000Z", usage = "200"),
      list(date = "2026-09-21T00:00:00.000Z", usage = "1000"),
      list(date = "2026-09-22T00:00:00.000Z", usage = "50")
    ))
  })
  out <- collect_messages(spend <- get_spend(days = 3, bearer_token = "tok"))

  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/usage/tweets?", fixed = TRUE)
  expect_match(urls[1], "days=3", fixed = TRUE)

  expect_s3_class(spend, "tbl_df")
  expect_equal(names(spend), c("date", "posts", "dollars"))
  expect_s3_class(spend$date, "Date")
  expect_equal(as.character(spend$date), c("2026-09-21", "2026-09-22", "2026-09-23"))
  expect_identical(spend$posts, c(1000L, 50L, 200L))
  expect_type(spend$dollars, "double")
  expect_equal(spend$dollars, c(1000, 50, 200) * 0.005)

  expect_equal(
    out$msgs,
    "Read 1,250 posts in the last 3 days, about $6.25 at the post price."
  )
})

test_that("get_spend follows the post price option", {
  withr::local_options(xapir.prices = list(posts = 0.01))
  httr2::local_mocked_responses(function(req) {
    usage_response(list(list(date = "2026-09-23T00:00:00.000Z", usage = "10")))
  })
  out <- collect_messages(spend <- get_spend(days = 1, bearer_token = "tok"))
  expect_equal(spend$dollars, 0.1)
  expect_equal(out$msgs, "Read 10 posts in the last 1 days, about $0.10 at the post price.")
})

test_that("get_spend with no daily usage gives a zero-row typed tibble", {
  httr2::local_mocked_responses(list(json_response(200, list(data = list()))))
  out <- collect_messages(spend <- get_spend(bearer_token = "tok"))
  expect_s3_class(spend, "tbl_df")
  expect_equal(nrow(spend), 0)
  expect_equal(names(spend), c("date", "posts", "dollars"))
  expect_s3_class(spend$date, "Date")
  expect_type(spend$posts, "integer")
  expect_type(spend$dollars, "double")
  expect_equal(out$msgs, "Read 0 posts in the last 7 days, about $0.00 at the post price.")
})

test_that("get_spend validates days and the token before any request", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_spend(days = 0, bearer_token = "tok"), "between 1 and 90")
  expect_error(get_spend(days = 91, bearer_token = "tok"), "between 1 and 90")
  expect_error(get_spend(days = "7", bearer_token = "tok"), "between 1 and 90")
  expect_error(get_spend(bearer_token = ""), "X_BEARER_TOKEN")
})
