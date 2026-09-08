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
  expect_error(get_timeline("tesla", max_results = 200, bearer_token = "tok"),
               "between 10 and 100")
  expect_error(get_recent_post("tesla", max_results = 5, bearer_token = "tok"),
               "between 10 and 100")
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
  expect_true(all(c("note_tweet", "article", "edit_controls", "possibly_sensitive")
                  %in% default_post_fields()))
  expect_true(all(c("is_identity_verified", "url") %in% default_user_fields()))
  expect_true("alt_text" %in% default_media_fields())
  expect_true("geo.place_id" %in% default_expansions())
  expect_false(any(c("contained_within", "name") %in% default_place_fields()))
})
