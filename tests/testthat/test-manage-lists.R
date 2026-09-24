# These test helpers mirror the ones at the top of test-write.R: testthat
# runs each file in its own environment, so they are repeated here.

# The signed-in account is "42" for every test here, with no users/me call.
use_me <- function(env = parent.frame()) {
  .x_env$my_user_id <- list(key = "tok", id = "42")
  withr::defer(.x_env$my_user_id <- NULL, envir = env)
}

test_that("create_list POSTs name, description and private to /lists", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(201, list(data = list(id = "5", name = "EV makers")))
  })

  expect_message(
    out <- withVisible(create_list("EV makers", description = "Who builds EVs")),
    "This request costs about \\$0.010"
  )

  expect_false(out$visible)
  expect_equal(out$value, list(id = "5", name = "EV makers"))
  req <- seen()[[1]]
  expect_equal(req$method, "POST")
  expect_match(req$url, "^https://api.x.com/2/lists$")
  expect_equal(
    sent_json(req),
    list(name = "EV makers", description = "Who builds EVs", private = FALSE)
  )
})

test_that("create_list leaves the description out when none is given", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(201, list(data = list(id = "5", name = "EV makers")))
  })
  suppressMessages(create_list("EV makers", private = TRUE))
  expect_equal(sent_json(seen()[[1]]), list(name = "EV makers", private = TRUE))
})

test_that("create_list checks the name, description and privacy first", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  expect_error(create_list(), "`name` is missing")
  expect_error(create_list(strrep("a", 26)), "1 to 25 characters")
  expect_error(create_list("ok", description = strrep("a", 101)), "100 characters")
  expect_error(create_list("ok", private = "yes"), "TRUE or FALSE")
})

test_that("update_list PUTs only the parts that were given", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(updated = TRUE)))
  })

  expect_message(out <- withVisible(update_list("5", name = "EVs")),
                 "This request costs about \\$0.005")

  expect_false(out$visible)
  expect_equal(out$value, list(updated = TRUE))
  req <- seen()[[1]]
  expect_equal(req$method, "PUT")
  expect_match(req$url, "/2/lists/5$")
  expect_equal(sent_json(req), list(name = "EVs"))
})

test_that("update_list needs something to change and passes a 403 on", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    json_response(403, list(
      title = "Forbidden", detail = "You do not own this list.", status = 403
    ))
  })
  expect_error(update_list("5"), "at least one of")
  expect_error(suppressMessages(update_list("5", private = TRUE)),
               "do not own this list")
})

test_that("delete_list DELETEs /lists/<id>", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(deleted = TRUE)))
  })
  expect_message(out <- delete_list("5"), "This request costs about \\$0.005")
  expect_equal(out, list(deleted = TRUE))
  expect_equal(seen()[[1]]$method, "DELETE")
  expect_match(seen()[[1]]$url, "/2/lists/5$")
})

test_that("add_list_member looks the handle up and POSTs the id", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    if (grepl("/users/by/username/tesla$", req$url)) {
      json_response(200, list(data = list(id = "2", username = "tesla")))
    } else {
      json_response(200, list(data = list(is_member = TRUE)))
    }
  })

  expect_message(out <- withVisible(add_list_member("5", username = "tesla")),
                 "This request costs about \\$0.005")

  expect_false(out$visible)
  expect_equal(out$value, list(is_member = TRUE))
  reqs <- seen()
  expect_equal(length(reqs), 2)
  expect_equal(reqs[[2]]$method, "POST")
  expect_match(reqs[[2]]$url, "/2/lists/5/members$")
  expect_equal(sent_json(reqs[[2]]), list(user_id = "2"))
})

test_that("remove_list_member with an id DELETEs /lists/<id>/members/<user_id>", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(is_member = FALSE)))
  })
  expect_message(out <- remove_list_member("5", user_id = "2"),
                 "This request costs about \\$0.005")
  expect_equal(out, list(is_member = FALSE))
  reqs <- seen()
  expect_equal(length(reqs), 1)
  expect_equal(reqs[[1]]$method, "DELETE")
  expect_match(reqs[[1]]$url, "/2/lists/5/members/2$")
})

test_that("list members need exactly one of handle and id", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  expect_error(add_list_member("5"), "either `username` or `user_id`")
  expect_error(remove_list_member("5", "a", "1"), "not both")
  expect_error(add_list_member(5, user_id = "1"), "`list_id` must be one string")
})

test_that("follow_list and unfollow_list use /users/<me>/followed_lists", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(following = req$method == "POST")))
  })

  expect_message(out <- withVisible(follow_list("5")),
                 "This request costs about \\$0.005")
  expect_false(out$visible)
  expect_equal(out$value, list(following = TRUE))
  expect_message(expect_equal(unfollow_list("5"), list(following = FALSE)),
                 "\\$0.005")

  reqs <- seen()
  expect_equal(reqs[[1]]$method, "POST")
  expect_match(reqs[[1]]$url, "/2/users/42/followed_lists$")
  expect_equal(sent_json(reqs[[1]]), list(list_id = "5"))
  expect_equal(reqs[[2]]$method, "DELETE")
  expect_match(reqs[[2]]$url, "/2/users/42/followed_lists/5$")
})

test_that("pin_list and unpin_list use /users/<me>/pinned_lists", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(pinned = req$method == "POST")))
  })

  expect_message(out <- withVisible(pin_list("5")),
                 "This request costs about \\$0.005")
  expect_false(out$visible)
  expect_equal(out$value, list(pinned = TRUE))
  expect_message(expect_equal(unpin_list("5"), list(pinned = FALSE)), "\\$0.005")

  reqs <- seen()
  expect_equal(reqs[[1]]$method, "POST")
  expect_match(reqs[[1]]$url, "/2/users/42/pinned_lists$")
  expect_equal(sent_json(reqs[[1]]), list(list_id = "5"))
  expect_equal(reqs[[2]]$method, "DELETE")
  expect_match(reqs[[2]]$url, "/2/users/42/pinned_lists/5$")
})

test_that("pin_list stops with the API's detail on a 403", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  httr2::local_mocked_responses(function(req) {
    json_response(403, list(
      title = "Forbidden", detail = "You can pin at most 5 lists.", status = 403
    ))
  })
  expect_error(suppressMessages(pin_list("5")), "at most 5 lists")
})

test_that("get_pinned_lists returns the lists table", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  seen <- record_requests(function(req) {
    json_response(200, list(
      data = list(
        list(id = "5", name = "EV makers", description = "Who builds EVs",
             created_at = "2026-09-01T10:00:00.000Z", follower_count = 3L,
             member_count = 10L, private = FALSE, owner_id = "42")
      ),
      meta = list(result_count = 1L)
    ))
  })

  expect_message(pinned <- get_pinned_lists(), "Reading up to 5 lists")

  expect_s3_class(pinned, "tbl_df")
  expect_equal(names(pinned), names(list_schema()))
  expect_equal(pinned$list_id, "5")
  expect_equal(pinned$list_name, "EV makers")
  expect_equal(pinned$member_count, 10L)
  expect_s3_class(pinned$created_at, "POSIXct")
  req <- seen()[[1]]
  expect_null(req$method)  # httr2 leaves a GET unset
  expect_match(req$url, "/2/users/42/pinned_lists\\?list.fields=")
})

test_that("get_pinned_lists with nothing pinned is the zero-row schema", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  use_me()
  httr2::local_mocked_responses(function(req) {
    json_response(200, list(meta = list(result_count = 0L)))
  })
  expect_identical(suppressMessages(get_pinned_lists()), list_schema())
})
