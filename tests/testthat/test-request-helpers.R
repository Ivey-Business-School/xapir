test_that("check_max_results accepts 10 to 100 and refuses the rest", {
  expect_silent(check_max_results(10))
  expect_silent(check_max_results(100))
  expect_error(check_max_results(9), "between 10 and 100")
  expect_error(check_max_results(101), "between 10 and 100")
  expect_error(check_max_results("a"), "between 10 and 100")
  expect_error(check_max_results(NA), "between 10 and 100")
})

test_that("a bad max_results stops before any request is made", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(
    get_timeline("tesla", max_results = 200, bearer_token = "tok"),
    "between 10 and 100"
  )
  expect_error(
    get_recent_post("tesla", max_results = 5, bearer_token = "tok"),
    "between 10 and 100"
  )
})

test_that("a missing token stops with advice", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_timeline("tesla", bearer_token = ""), "X_BEARER_TOKEN")
  expect_error(get_recent_post("tesla", bearer_token = ""), "X_BEARER_TOKEN")
  expect_error(get_post("1", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("only 429 and 5xx are transient", {
  expect_true(x_is_transient(httr2::response(429)))
  expect_true(x_is_transient(httr2::response(500)))
  expect_true(x_is_transient(httr2::response(503)))
  expect_false(x_is_transient(httr2::response(400)))
  expect_false(x_is_transient(httr2::response(401)))
  expect_false(x_is_transient(httr2::response(403)))
  expect_false(x_is_transient(httr2::response(404)))
  expect_false(x_is_transient(httr2::response(200)))
})

test_that("the wait honours Retry-After, then x-rate-limit-reset, else backs off", {
  expect_equal(x_retry_after(httr2::response(429, headers = list(`Retry-After` = "7"))), 7)
  reset <- as.character(round(as.numeric(Sys.time())) + 90)
  wait <- x_retry_after(httr2::response(429, headers = list(`x-rate-limit-reset` = reset)))
  expect_true(wait >= 88 && wait <= 91)
  expect_true(is.na(x_retry_after(httr2::response(429))))
})

test_that("a 4xx stops at once with the API's own message", {
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    json_response(401, list(title = "Unauthorized", detail = "Unauthorized", status = 401))
  })
  expect_error(
    suppressMessages(get_timeline("tesla", bearer_token = "bad")),
    "Unauthorized"
  )
  expect_equal(calls, 1)
})

test_that("an unknown handle stops with the API's reason", {
  httr2::local_mocked_responses(list(
    json_response(200, list(errors = list(list(
      title = "Not Found Error",
      detail = "Could not find user with username: [nobodyhere]."
    ))))
  ))
  expect_error(
    suppressMessages(get_timeline("nobodyhere", bearer_token = "tok")),
    "Could not find user with username"
  )
})

test_that("get_timeline announces the cap and never returns more than max_posts", {
  user <- json_response(200, list(data = list(id = "42", username = "tesla")))
  httr2::local_mocked_responses(list(
    user,
    posts_page(1:100, "n2"),
    posts_page(101:200, "n3"),
    posts_page(201:300)
  ))
  msgs <- character(0)
  pages <- withCallingHandlers(
    get_timeline("tesla", max_posts = 150, sleep_time = 0, bearer_token = "tok"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_match(msgs[1], "Reading up to 150 posts, about \\$0.75", fixed = FALSE)
  expect_equal(length(pages), 2)
  expect_equal(sum(vapply(pages, function(p) length(p$data), 1L)), 150)
})

test_that("get_recent_post never returns more than max_posts", {
  httr2::local_mocked_responses(list(
    posts_page(1:100, "n2"),
    posts_page(101:200, "n3")
  ))
  pages <- suppressMessages(
    get_recent_post("tesla", max_posts = 120, bearer_token = "tok")
  )
  expect_equal(sum(vapply(pages, function(p) length(p$data), 1L)), 120)
})

test_that("the default post fields include the long-post and article fields", {
  long_post_fields <- c(
    "note_tweet", "article", "edit_controls", "possibly_sensitive"
  )
  expect_true(all(long_post_fields %in% default_post_fields()))
  expect_true(all(c("is_identity_verified", "url") %in% default_user_fields()))
  expect_true("alt_text" %in% default_media_fields())
  expect_true("geo.place_id" %in% default_expansions())
  expect_false(any(c("contained_within", "name") %in% default_place_fields()))
})

test_that("get_timeline by user_id makes no user read", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    posts_page(1:10)
  })
  pages <- suppressMessages(
    get_timeline(user_id = "42", max_posts = 10, bearer_token = "tok")
  )
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/users/42/tweets", fixed = TRUE)
  expect_false(any(grepl("/users/by/username", urls, fixed = TRUE)))
  expect_equal(length(pages[[1]]$data), 10)
})

test_that("get_timeline by username still pays the lookup first", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("/users/by/username", req$url, fixed = TRUE)) {
      json_response(200, list(data = list(id = "42", username = "tesla")))
    } else {
      posts_page(1:10)
    }
  })
  suppressMessages(
    get_timeline("tesla", max_posts = 10, bearer_token = "tok")
  )
  expect_equal(length(urls), 2)
  expect_match(urls[1], "/users/by/username/tesla", fixed = TRUE)
  expect_match(urls[2], "/users/42/tweets", fixed = TRUE)
})

test_that("get_timeline wants exactly one of username and user_id", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(
    get_timeline(bearer_token = "tok"),
    "either `username` or `user_id`"
  )
  expect_error(
    get_timeline("tesla", user_id = "42", bearer_token = "tok"),
    "either `username` or `user_id`"
  )
  expect_error(
    get_timeline(user_id = 42, bearer_token = "tok"),
    "string of digits"
  )
  expect_error(
    get_timeline(user_id = "tesla", bearer_token = "tok"),
    "string of digits"
  )
})

test_that("check_max_posts wants a finite number of 1 or more and names the argument", {
  expect_silent(check_max_posts(1))
  expect_silent(check_max_posts(500))
  expect_error(check_max_posts(Inf), "`max_posts` must be a finite number")
  expect_error(check_max_posts(0), "`max_posts` must be a finite number")
  expect_error(check_max_posts(NA), "`max_posts` must be a finite number")
  expect_error(check_max_posts("a"), "`max_posts` must be a finite number")
  expect_error(check_max_posts(Inf, arg = "max_users"), "`max_users` must be a finite number")
  expect_error(check_max_posts(0, arg = "max_users"), "`max_users` must be a finite number")
  expect_silent(check_max_posts(20, arg = "max_users"))
})

test_that("check_post_ids keeps ids as digit strings and counts them", {
  expect_silent(check_post_ids("1234567890123456789"))
  expect_silent(check_post_ids(as.character(1:100)))
  expect_error(check_post_ids(as.character(1:101)), "at most 100 ids")
  expect_error(check_post_ids(1234), "strings of digits")
  expect_error(check_post_ids("abc"), "strings of digits")
  expect_error(check_post_ids(character(0)), "strings of digits")
  expect_error(check_post_ids(c("1", NA)), "strings of digits")
  expect_error(check_post_ids(c("1", "2"), max_ids = 1, arg = "post_id"), "`post_id` can hold at most 1 id")
  expect_error(check_post_ids(20, max_ids = 1, arg = "post_id"), "`post_id` must be one string of digits")
})

test_that("announce_cap reads the price from the options", {
  expect_message(announce_cap(150), "Reading up to 150 posts, about \\$0.75. Set max_posts")
  expect_message(announce_cap(20, what = "users"), "Reading up to 20 users, about \\$0.20. Set max_users")
  expect_message(announce_cap(100, arg = "max_results"), "Set max_results to change this")
  expect_message(announce_cap(2000), "Reading up to 2,000 posts, about \\$10.00")
  op <- options(xapir.price_per_post = 0.02, xapir.price_per_user = 0.05)
  on.exit(options(op), add = TRUE)
  expect_message(announce_cap(150), "about \\$3.00")
  expect_message(announce_cap(20, what = "users"), "about \\$1.00")
  expect_message(announce_cap(150, price = 0.001), "about \\$0.15")
  expect_message(announce_total(150), "^Read 150 posts, about \\$3\\.00\\.")
  expect_message(announce_total(20, what = "users"), "^Read 20 users, about \\$1\\.00\\.")
})

test_that("check_query and check_granularity stop on what the API would reject", {
  expect_silent(check_query("#marketing"))
  expect_error(check_query(""), "`query` must be one search string")
  expect_error(check_query(c("a", "b")), "`query` must be one search string")
  expect_error(check_query(NULL), "`query` must be one search string")
  expect_silent(check_granularity("day"))
  expect_error(check_granularity("week"), "\"minute\", \"hour\" or \"day\"")
})

test_that("prices follow the pricing page and the xapir.prices option", {
  expect_equal(x_price("posts"), 0.005)
  expect_equal(x_price("users"), 0.010)
  expect_equal(x_price("follows"), 0.010)
  expect_equal(x_price("likes"), 0.001)
  expect_equal(x_price("blocks"), 0.001)
  expect_equal(x_price("counts_recent"), 0.005)
  expect_equal(x_price("trends"), 0.010)
  expect_equal(x_price("post_create"), 0.015)
  expect_error(x_price("nonsense"), "No price is known")
  op <- options(xapir.prices = list(follows = 0.02))
  on.exit(options(op), add = TRUE)
  expect_equal(x_price("follows"), 0.02)
  expect_equal(x_price("posts"), 0.005)
})

test_that("check_max_results takes the endpoint's own range", {
  expect_silent(check_max_results(1000, min = 1, max = 1000, what = "users"))
  expect_error(check_max_results(1001, min = 1, max = 1000, what = "users"),
               "between 1 and 1000")
  expect_error(check_max_results(5), "between 10 and 100")
})

test_that("announce_request_cost prints one line per request", {
  expect_message(announce_request_cost("trends"), "costs about \\$0.010")
  expect_message(announce_request_cost("post_create", n = 3), "3 requests, about \\$0.04")
})
