# Every test here mocks the API. Nothing calls X.

# Caps ---------------------------------------------------------------------

test_that("max_posts = Inf stops before any request", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(
    get_timeline("tesla", max_posts = Inf, bearer_token = "tok"),
    "`max_posts` must be a finite number"
  )
  expect_error(
    get_recent_post("tesla", max_posts = Inf, bearer_token = "tok"),
    "`max_posts` must be a finite number"
  )
  expect_error(
    get_mentions("tesla", max_posts = Inf, bearer_token = "tok"),
    "`max_posts` must be a finite number"
  )
  mock_user_token()
  expect_error(
    get_liking_users("20", max_users = Inf),
    "`max_users` must be a finite number"
  )
  expect_error(
    get_bookmark(max_posts = Inf),
    "`max_posts` must be a finite number"
  )
})

test_that("the option price changes the announced dollars", {
  httr2::local_mocked_responses(list(posts_page(1:10)))
  op <- options(xapir.price_per_post = 0.01)
  on.exit(options(op), add = TRUE)
  out <- collect_messages(
    get_recent_post("tesla", max_posts = 100, bearer_token = "tok")
  )
  expect_match(out$msgs[1], "Reading up to 100 posts, about \\$1.00", fixed = FALSE)
  expect_match(out$msgs[length(out$msgs)], "^Read 10 posts, about \\$0.10\\.$")
})

test_that("a truncated pull reports the total it read and spent", {
  httr2::local_mocked_responses(list(
    posts_page(1:100, "n2"),
    posts_page(101:200, "n3"),
    posts_page(201:300, "n4")
  ))
  out <- collect_messages(
    get_recent_post("tesla", max_posts = 150, bearer_token = "tok")
  )
  expect_match(out$msgs[1], "Reading up to 150 posts, about \\$0.75")
  expect_equal(out$msgs[2], "Finished getting posts on page 1")
  expect_equal(out$msgs[3], "Finished getting posts on page 2")
  expect_equal(out$msgs[4], "Read 150 posts, about $0.75.")
  expect_equal(length(out$msgs), 4)
  expect_equal(sum(vapply(out$result, function(p) length(p$data), 1L)), 150)
})

test_that("a pull the API cut short reports what it actually read", {
  httr2::local_mocked_responses(list(posts_page(1:10)))
  out <- collect_messages(
    get_recent_post("tesla", max_posts = 500, bearer_token = "tok")
  )
  expect_match(out$msgs[1], "Reading up to 500 posts, about \\$2.50")
  expect_equal(out$msgs[length(out$msgs)], "Read 10 posts, about $0.05.")
})

test_that("get_liking_users announces users and their price", {
  mock_user_token()
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    posts_page(1:20)
  })
  out <- collect_messages(get_liking_users("20", max_users = 20))
  # who liked a post is a like read: $0.001 each, not the $0.010 of a user read
  expect_match(out$msgs[1], "Reading up to 20 users, about \\$0.02. Set max_users")
  expect_equal(out$msgs[2], "Finished getting users on page 1")
  expect_equal(out$msgs[3], "Read 20 users, about $0.02.")
  expect_match(urls[1], "/tweets/20/liking_users", fixed = TRUE)
  # a users table, like every other user reader, not raw pages
  expect_s3_class(out$result, "tbl_df")
  expect_equal(nrow(out$result), 20)
  expect_equal(names(out$result), names(user_schema()))

  op <- options(xapir.prices = list(likes = 0.05))
  on.exit(options(op), add = TRUE)
  out <- collect_messages(get_liking_users("20", max_users = 20))
  expect_match(out$msgs[1], "about \\$1.00")
})

test_that("get_liking_users refuses a post_id that is not a digit string", {
  mock_user_token()
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_liking_users(20), "`post_id` must be one string of digits")
  expect_error(get_liking_users("abc"), "`post_id` must be one string of digits")
})

# Defaults -------------------------------------------------------------------

test_that("get_recent_post reads 500 posts by default, like get_timeline", {
  expect_equal(formals(get_recent_post)$max_posts, 500)
  expect_equal(formals(get_timeline)$max_posts, 500)
  expect_equal(formals(get_mentions)$max_posts, 500)
})

test_that("no reader pauses between pages by default", {
  readers <- list(
    get_timeline, get_recent_post, get_mentions, get_account_timeline,
    get_bookmark, get_liked_posts, get_liking_users
  )
  for (reader in readers) {
    expect_equal(formals(reader)$sleep_time, 0)
  }
})

# Single-page readers --------------------------------------------------------

test_that("get_quote_post announces the worst case and returns one page", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    posts_page(1:10)
  })
  out <- collect_messages(
    get_quote_post("20", max_results = 50, bearer_token = "tok")
  )
  expect_equal(length(out$msgs), 1)
  expect_match(out$msgs[1], "Reading up to 50 posts, about \\$0.25. Set max_results")
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/tweets/20/quote_tweets", fixed = TRUE)
  expect_match(urls[1], "max_results=50", fixed = TRUE)
  expect_equal(length(out$result), 1)
  expect_equal(length(out$result[[1]]$data), 10)
})

test_that("get_quote_post and get_repost stop before a request on bad input", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_quote_post("20", max_results = 5, bearer_token = "tok"), "between 10 and 100")
  expect_error(get_quote_post(20, bearer_token = "tok"), "`post_id` must be one string of digits")
  expect_error(get_quote_post("20", bearer_token = ""), "X_BEARER_TOKEN")
  expect_error(get_repost("20", max_results = 500, bearer_token = "tok"), "between 10 and 100")
  expect_error(get_repost("x", bearer_token = "tok"), "`post_id` must be one string of digits")
})

test_that("get_repost announces the worst case and returns one page", {
  httr2::local_mocked_responses(list(posts_page(1:10)))
  out <- collect_messages(get_repost("20", bearer_token = "tok"))
  expect_match(out$msgs[1], "Reading up to 100 posts, about \\$0.50. Set max_results")
  expect_equal(length(out$result), 1)
})

test_that("get_repost_of_me checks max_results and announces the worst case", {
  mock_user_token()
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_repost_of_me(max_results = 1000), "between 10 and 100")

  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    posts_page(1:10)
  })
  out <- collect_messages(get_repost_of_me(max_results = 10))
  expect_match(out$msgs[1], "Reading up to 10 posts, about \\$0.05")
  expect_match(urls[1], "/users/reposts_of_me", fixed = TRUE)
  expect_equal(length(out$result), 1)
})

test_that("get_post validates the ids before the request and announces as many posts", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_post(as.character(1:101), bearer_token = "tok"), "at most 100 ids")
  expect_error(get_post(c("1", "two"), bearer_token = "tok"), "strings of digits")
  expect_error(get_post(1234567890123456789, bearer_token = "tok"), "strings of digits")
  expect_error(get_post(character(0), bearer_token = "tok"), "strings of digits")

  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    posts_page(1:4)
  })
  out <- collect_messages(get_post(c("1", "2", "3", "4"), bearer_token = "tok"))
  expect_equal(length(out$msgs), 1)
  expect_match(out$msgs[1], "Reading up to 4 posts, about \\$0.02. Set post_ids")
  expect_match(urls[1], "ids=1%2C2%2C3%2C4", fixed = TRUE)
  expect_equal(length(out$result), 1)
  expect_equal(length(out$result[[1]]$data), 4)
})

# Counts ---------------------------------------------------------------------

test_that("get_recent_post_count returns a tibble of periods and says counts are free", {
  httr2::local_mocked_responses(list(count_page(list(
    list(start = "2026-09-01T09:30:00.000Z", end = "2026-09-01T10:00:00.000Z", tweet_count = 2L),
    list(start = "2026-09-01T10:00:00.000Z", end = "2026-09-01T11:00:00.000Z", tweet_count = 5L),
    list(start = "2026-09-01T11:00:00.000Z", end = "2026-09-01T12:00:00.000Z", tweet_count = 7L),
    list(start = "2026-09-01T12:00:00.000Z", end = "2026-09-01T12:15:00.000Z", tweet_count = 1L)
  ))))
  out <- collect_messages(
    get_recent_post_count("tesla", is_local_tz = FALSE, bearer_token = "tok")
  )
  # a count is billed once per request, however many posts it counts
  expect_match(out$msgs[1], "This request costs about \\$0.005")
  counts <- out$result
  expect_s3_class(counts, "tbl_df")
  expect_equal(names(counts), c("start", "end", "post_count"))
  expect_equal(nrow(counts), 2)
  expect_s3_class(counts$start, "POSIXct")
  expect_s3_class(counts$end, "POSIXct")
  expect_type(counts$post_count, "integer")
  expect_equal(counts$post_count, c(5L, 7L))
  expect_equal(format(counts$start[1], "%Y-%m-%d %H:%M", tz = "UTC"), "2026-09-01 10:00")
})

test_that("get_recent_post_count keeps every row when drop_incomplete is FALSE", {
  httr2::local_mocked_responses(list(count_page(list(
    list(start = "2026-09-01T09:30:00.000Z", end = "2026-09-01T10:00:00.000Z", tweet_count = 2L),
    list(start = "2026-09-01T10:00:00.000Z", end = "2026-09-01T10:15:00.000Z", tweet_count = 5L)
  ))))
  counts <- suppressMessages(get_recent_post_count(
    "tesla", drop_incomplete = FALSE, is_local_tz = FALSE, bearer_token = "tok"
  ))
  expect_equal(counts$post_count, c(2L, 5L))
})

test_that("get_recent_post_count returns the empty schema when there is no data", {
  httr2::local_mocked_responses(list(
    json_response(200, list(data = list(), meta = list(total_tweet_count = 0L))),
    json_response(200, list(meta = list(total_tweet_count = 0L)))
  ))
  for (i in 1:2) {
    counts <- suppressMessages(get_recent_post_count("nothingmatchesthis", bearer_token = "tok"))
    expect_s3_class(counts, "tbl_df")
    expect_equal(nrow(counts), 0)
    expect_equal(names(counts), c("start", "end", "post_count"))
    expect_s3_class(counts$start, "POSIXct")
    expect_s3_class(counts$end, "POSIXct")
    expect_type(counts$post_count, "integer")
  }
})

test_that("get_recent_post_count stops before a request on bad input", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_recent_post_count("", bearer_token = "tok"), "`query` must be one search string")
  expect_error(get_recent_post_count("tesla", granularity = "week", bearer_token = "tok"), "\"minute\", \"hour\" or \"day\"")
  expect_error(get_recent_post_count("tesla", bearer_token = ""), "X_BEARER_TOKEN")
})

# iso_8601 -------------------------------------------------------------------

test_that("iso_8601 turns dates, date-times and strings into UTC ISO 8601", {
  iso <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"

  expect_equal(iso_8601(as.Date("2024-07-01")), "2024-07-01T00:00:00Z")
  expect_match(iso_8601(as.Date("2024-07-01")), iso)

  at <- as.POSIXct("2024-07-01 15:00:00", tz = "America/New_York")
  expect_equal(iso_8601(at), "2024-07-01T19:00:00Z")
  expect_match(iso_8601(at), iso)

  expect_equal(iso_8601("2024-07-01", tz = "America/New_York"), "2024-07-01T04:00:00Z")
  expect_equal(iso_8601("2024-07-01 15:00:00", tz = "America/New_York"), "2024-07-01T19:00:00Z")
  expect_equal(iso_8601("2024-07-01", tz = "UTC"), "2024-07-01T00:00:00Z")
  expect_match(iso_8601("2024-07-01", tz = "Asia/Tokyo"), iso)
  expect_equal(iso_8601("2024-07-01", tz = "Asia/Tokyo"), "2024-06-30T15:00:00Z")

  expect_match(iso_8601(Sys.time()), iso)
  expect_match(iso_8601(Sys.Date()), iso)
})

test_that("get_liking_users gives the zero-row users table for a post nobody liked", {
  mock_user_token()
  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  out <- suppressMessages(get_liking_users("20", max_users = 20))
  expect_identical(out, user_schema())
})

test_that("your own data is announced at the owned price", {
  # ten of your own posts are $0.01, not $0.05, once the package knows your id
  op <- options(xapir.my_user_id = "42")
  on.exit(options(op), add = TRUE)
  httr2::local_mocked_responses(list(posts_page(1:10)))
  out <- collect_messages(
    get_timeline(user_id = "42", max_posts = 10, bearer_token = "tok")
  )
  expect_match(out$msgs[1], "Reading up to 10 posts, about \\$0.01 \\(your own data\\)")
  expect_equal(out$msgs[length(out$msgs)], "Read 10 posts, about $0.01.")

  # somebody else's posts stay at the post price
  httr2::local_mocked_responses(list(posts_page(1:10)))
  out <- collect_messages(
    get_timeline(user_id = "43", max_posts = 10, bearer_token = "tok")
  )
  expect_match(out$msgs[1], "Reading up to 10 posts, about \\$0.05\\. Set max_posts")
})

test_that("bookmarks are always your own and warn when a username is given", {
  mock_user_token()
  httr2::local_mocked_responses(function(req) {
    if (grepl("/users/me", req$url, fixed = TRUE)) {
      json_response(200, list(data = list(id = "42", username = "me")))
    } else {
      posts_page(1:10)
    }
  })
  out <- collect_messages(get_bookmark(max_posts = 10))
  expect_match(out$msgs[1], "about \\$0.01 \\(your own data\\)")
  expect_warning(suppressMessages(get_bookmark("tesla", max_posts = 10)), "ignored by get_bookmark")
})

test_that("get_post_analytics explains a 403 in plain words", {
  mock_user_token()
  httr2::local_mocked_responses(list(
    json_response(403, list(title = "Client Forbidden", detail = "attached to a Project"))
  ))
  expect_error(
    get_post_analytics("1", start_time = "2026-09-01T00:00:00Z", end_time = "2026-09-02T00:00:00Z"),
    "not open to this account"
  )
})
