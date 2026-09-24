# These test helpers mirror the ones at the top of test-write.R: testthat
# runs each file in its own environment, so they are repeated here.

# The signed-in account is "42" for every test here, with no users/me call.
use_me <- function(env = parent.frame()) {
  .x_env$my_user_id <- list(key = "tok", id = "42")
  withr::defer(.x_env$my_user_id <- NULL, envir = env)
}

test_that("like_post POSTs the post id to /users/<me>/likes and says the cost", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(liked = TRUE)))
  })

  expect_message(out <- withVisible(like_post("20")),
                 "This request costs about \\$0.015")

  expect_false(out$visible)
  expect_equal(out$value, list(liked = TRUE))
  req <- seen()[[1]]
  expect_equal(length(seen()), 1)
  expect_equal(req$method, "POST")
  expect_match(req$url, "^https://api.x.com/2/users/42/likes$")
  expect_equal(sent_json(req), list(tweet_id = "20"))
  expect_equal(auth_header(req), "Bearer tok")
})

test_that("unlike_post DELETEs /users/<me>/likes/<post_id>", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(liked = FALSE)))
  })

  expect_message(out <- withVisible(unlike_post("20")),
                 "This request costs about \\$0.010")

  expect_false(out$visible)
  expect_equal(out$value, list(liked = FALSE))
  req <- seen()[[1]]
  expect_equal(req$method, "DELETE")
  expect_match(req$url, "/2/users/42/likes/20$")
  expect_null(req$body)
})

test_that("like_post stops with the API's detail on a 403", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  httr2::local_mocked_responses(function(req) {
    json_response(403, list(
      title = "Forbidden", detail = "You cannot like this post.", status = 403
    ))
  })
  expect_error(suppressMessages(like_post("20")), "cannot like this post")
})

test_that("post ids for likes must be strings of digits", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  expect_error(like_post(20), "string of digits")
  expect_error(unlike_post("abc"), "string of digits")
})

test_that("block_user looks a handle up and POSTs the id to /users/<me>/blocking", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  seen <- record_requests(function(req) {
    if (grepl("/users/by/username/spammer$", req$url)) {
      json_response(200, list(data = list(id = "2", username = "spammer")))
    } else {
      json_response(200, list(data = list(blocking = TRUE)))
    }
  })

  expect_message(out <- withVisible(block_user(target_username = "@spammer")),
                 "This request costs about \\$0.015")

  expect_false(out$visible)
  expect_equal(out$value, list(blocking = TRUE))
  reqs <- seen()
  expect_equal(length(reqs), 2)
  expect_match(reqs[[1]]$url, "/users/by/username/spammer$")
  expect_equal(reqs[[2]]$method, "POST")
  expect_match(reqs[[2]]$url, "/2/users/42/blocking$")
  expect_equal(sent_json(reqs[[2]]), list(target_user_id = "2"))
})

test_that("unblock_user with an id skips the lookup and DELETEs", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(blocking = FALSE)))
  })

  expect_message(out <- unblock_user(target_user_id = "2"),
                 "This request costs about \\$0.010")

  expect_equal(out, list(blocking = FALSE))
  reqs <- seen()
  expect_equal(length(reqs), 1)
  expect_equal(reqs[[1]]$method, "DELETE")
  expect_match(reqs[[1]]$url, "/2/users/42/blocking/2$")
})

test_that("block_user stops with the API's detail on a 403", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  httr2::local_mocked_responses(function(req) {
    json_response(403, list(
      title = "Forbidden",
      detail = "This endpoint is only available on the Enterprise plan.",
      status = 403
    ))
  })
  expect_error(suppressMessages(block_user(target_user_id = "2")),
               "Enterprise plan")
})

test_that("block_user needs exactly one of handle and id, before any request", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  expect_error(block_user(), "either `target_username` or `target_user_id`")
  expect_error(block_user("a", "1"), "not both")
  expect_error(unblock_user(target_user_id = 2), "string of digits")
  expect_error(unblock_user(target_username = ""), "one handle")
})
