# Every write function needs a user token. authenticate_user() opens a
# browser, so it is stubbed to return a token-shaped list; the requests it
# would sign are then mocked through httr2.

fake_token <- function(...) list(access_token = "tok")

# Records every request the mock sees so a test can check the method, path
# and body. `respond` maps a request to a response.
record_requests <- function(respond) {
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1]] <<- req
    respond(req)
  }, env = parent.frame())
  function() seen
}

# httr2 keeps the R object given to req_body_json() in req$body$data and
# serialises it when the request is performed, so the mock sees the list.
sent_json <- function(req) {
  data <- req$body$data
  if (is.character(data)) {
    jsonlite::fromJSON(data, simplifyVector = FALSE)
  } else {
    data
  }
}

test_that("the scopes cover every endpoint the package exposes", {
  needed <- c(
    "tweet.read", "tweet.write", "tweet.moderate.write", "users.read",
    "follows.read", "follows.write", "like.read", "list.read", "block.read",
    "mute.read", "mute.write", "bookmark.read", "bookmark.write",
    "offline.access"
  )
  expect_true(all(needed %in% x_oauth_scopes))
  expect_false(any(duplicated(x_oauth_scopes)))
  expect_equal(x_oauth_scope(), paste(x_oauth_scopes, collapse = " "))
})

test_that("authenticate_user stops with advice when the client id is empty", {
  expect_error(authenticate_user(client_id = ""), "X_CLIENT_ID")
  expect_error(authenticate_user(client_id = ""), ".Renviron")
  expect_error(authenticate_user(client_id = NA_character_), "X_CLIENT_ID")
})

test_that("create_post sends a POST to /2/tweets and returns data invisibly", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(201, list(data = list(id = "99", text = "Hello, world!")))
  })

  out <- withVisible(create_post("Hello, world!"))

  expect_false(out$visible)
  expect_equal(out$value, list(id = "99", text = "Hello, world!"))

  req <- seen()[[1]]
  expect_equal(length(seen()), 1)
  expect_equal(req$method, "POST")
  expect_match(req$url, "^https://api.x.com/2/tweets$")
  expect_equal(sent_json(req), list(text = "Hello, world!"))
  expect_equal(req$headers$Authorization, "Bearer tok")
})

test_that("create_post lets the API judge length and passes its message on", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    json_response(403, list(
      title = "Forbidden", detail = "Your Tweet text is too long.", status = 403
    ))
  })
  long_text <- strrep("a", 300)
  expect_error(create_post(long_text), "too long")
})

test_that("create_post only sends the optional parts that were given", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(201, list(data = list(id = "1", text = "poll")))
  })
  create_post("poll", poll = list(options = c("a", "b"), duration_minutes = 60),
              nullcast = TRUE)
  body <- sent_json(seen()[[1]])
  expect_setequal(names(body), c("text", "nullcast", "poll"))
  expect_true(body$nullcast)
  expect_equal(body$poll$duration_minutes, 60)
})

test_that("create_repost hits /users/<me>/retweets and fetches users/me once", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  .x_env$my_user_id <- NULL
  withr::defer(.x_env$my_user_id <- NULL)

  seen <- record_requests(function(req) {
    if (grepl("/users/me$", req$url)) {
      json_response(200, list(data = list(id = "42", username = "me")))
    } else {
      json_response(200, list(data = list(retweeted = TRUE)))
    }
  })

  first  <- withVisible(create_repost("20"))
  second <- create_repost("21")

  expect_false(first$visible)
  expect_equal(first$value, list(retweeted = TRUE))
  expect_equal(second, list(retweeted = TRUE))

  urls <- vapply(seen(), function(r) r$url, "")
  expect_equal(sum(grepl("/users/me$", urls)), 1)
  expect_equal(sum(grepl("/users/42/retweets$", urls)), 2)

  repost <- seen()[[2]]
  expect_equal(repost$method, "POST")
  expect_equal(sent_json(repost), list(tweet_id = "20"))
})

test_that("my_user_id refetches when the token changes", {
  .x_env$my_user_id <- NULL
  withr::defer(.x_env$my_user_id <- NULL)
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    json_response(200, list(data = list(id = as.character(calls))))
  })
  expect_equal(my_user_id(list(access_token = "a")), "1")
  expect_equal(my_user_id(list(access_token = "a")), "1")
  expect_equal(my_user_id(list(access_token = "b")), "2")
  expect_equal(calls, 2)
})

test_that("delete_repost sends DELETE to /users/<me>/retweets/<post_id>", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  .x_env$my_user_id <- list(key = "tok", id = "42")
  withr::defer(.x_env$my_user_id <- NULL)
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(retweeted = FALSE)))
  })
  expect_equal(delete_repost("20"), list(retweeted = FALSE))
  req <- seen()[[1]]
  expect_equal(req$method, "DELETE")
  expect_match(req$url, "/users/42/retweets/20$")
})

test_that("delete_post returns one row per id, mixing success and failure", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    if (grepl("/tweets/2$", req$url)) {
      json_response(404, list(
        title = "Not Found Error",
        detail = "Could not find tweet with id: [2].",
        status = 404
      ))
    } else {
      json_response(200, list(data = list(deleted = TRUE)))
    }
  })

  out <- delete_post(c("1", "2", "3"), sleep_time = 0)

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("post_id", "deleted", "error"))
  expect_equal(out$post_id, c("1", "2", "3"))
  expect_equal(out$deleted, c(TRUE, FALSE, TRUE))
  expect_true(is.na(out$error[1]))
  expect_match(out$error[2], "Could not find tweet")
  expect_true(is.na(out$error[3]))

  reqs <- seen()
  expect_equal(length(reqs), 3)
  expect_true(all(vapply(reqs, function(r) r$method, "") == "DELETE"))
  expect_match(reqs[[1]]$url, "/2/tweets/1$")
})

test_that("delete_post pauses between batches and not after the last", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    json_response(200, list(data = list(deleted = TRUE)))
  })
  msgs <- character(0)
  out <- withCallingHandlers(
    delete_post(as.character(1:5), sleep_time = 0, batch_size = 2),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  # One cost line, then five ids in batches of two make three batches, so
  # two pauses.
  expect_equal(nrow(out), 5)
  expect_true(all(out$deleted))
  expect_equal(length(msgs), 3)
  expect_match(msgs[1], "5 requests, about \\$")
  expect_match(msgs[2], "batch 1 of 3")
  expect_match(msgs[3], "batch 2 of 3")
})

test_that("unfollow_user throws with the API detail on 403", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    if (grepl("/users/by/username/tesla$", req$url)) {
      json_response(200, list(data = list(id = "1", username = "tesla")))
    } else if (grepl("/users/by/username/elonmusk$", req$url)) {
      json_response(200, list(data = list(id = "2", username = "elonmusk")))
    } else {
      json_response(403, list(
        title = "Forbidden",
        detail = "You are not following this user.",
        status = 403
      ))
    }
  })
  expect_error(unfollow_user("tesla", "elonmusk"), "not following this user")
})

test_that("follow_user looks up both ids and posts the target id", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    if (grepl("/users/by/username/tesla$", req$url)) {
      json_response(200, list(data = list(id = "1", username = "tesla")))
    } else if (grepl("/users/by/username/elonmusk$", req$url)) {
      json_response(200, list(data = list(id = "2", username = "elonmusk")))
    } else {
      json_response(200, list(data = list(following = TRUE, pending_follow = FALSE)))
    }
  })
  out <- follow_user("tesla", "elonmusk")
  expect_equal(out, list(following = TRUE, pending_follow = FALSE))
  follow <- seen()[[3]]
  expect_equal(follow$method, "POST")
  expect_match(follow$url, "/users/1/following$")
  expect_equal(sent_json(follow), list(target_user_id = "2"))
})

test_that("mute and unmute use the muting endpoint", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    if (grepl("/users/by/username/", req$url)) {
      id <- if (grepl("me$", req$url)) "1" else "2"
      json_response(200, list(data = list(id = id)))
    } else if (req$method == "POST") {
      json_response(200, list(data = list(muting = TRUE)))
    } else {
      json_response(200, list(data = list(muting = FALSE)))
    }
  })
  expect_equal(mute_user("me", "them"), list(muting = TRUE))
  expect_equal(unmute_user("me", "them"), list(muting = FALSE))
  urls <- vapply(seen(), function(r) r$url, "")
  expect_match(urls[3], "/users/1/muting$")
  expect_match(urls[6], "/users/1/muting/2$")
  expect_equal(seen()[[6]]$method, "DELETE")
})

test_that("hide_reply PUTs the hidden flag", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(hidden = sent_json(req)$hidden)))
  })
  expect_equal(hide_reply("5"), list(hidden = TRUE))
  expect_equal(hide_reply("5", hidden = FALSE), list(hidden = FALSE))
  req <- seen()[[1]]
  expect_equal(req$method, "PUT")
  expect_match(req$url, "/2/tweets/5/hidden$")
  expect_equal(sent_json(req), list(hidden = TRUE))
})

test_that("bookmarks act on the signed-in account and ignore username", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  .x_env$my_user_id <- list(key = "tok", id = "42")
  withr::defer(.x_env$my_user_id <- NULL)
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(bookmarked = req$method == "POST")))
  })

  expect_equal(create_bookmark("7"), list(bookmarked = TRUE))
  expect_equal(delete_bookmark("7"), list(bookmarked = FALSE))
  expect_warning(create_bookmark("7", username = "Tesla"), "ignored")

  reqs <- seen()
  expect_equal(reqs[[1]]$method, "POST")
  expect_match(reqs[[1]]$url, "/users/42/bookmarks$")
  expect_equal(sent_json(reqs[[1]]), list(tweet_id = "7"))
  expect_equal(reqs[[2]]$method, "DELETE")
  expect_match(reqs[[2]]$url, "/users/42/bookmarks/7$")
  expect_false(any(grepl("/users/by/username", vapply(reqs, function(r) r$url, ""))))
})

test_that("the tweet_id alias warns and works", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  .x_env$my_user_id <- list(key = "tok", id = "42")
  withr::defer(.x_env$my_user_id <- NULL)
  seen <- record_requests(function(req) {
    json_response(200, list(data = list(retweeted = TRUE, bookmarked = TRUE)))
  })

  expect_warning(create_repost(tweet_id = "20"), "`tweet_id` is deprecated")
  expect_warning(delete_repost(tweet_id = "20"), "use `post_id`")
  expect_warning(create_bookmark(tweet_id = "20"), "deprecated")
  expect_warning(delete_bookmark(tweet_id = "20"), "deprecated")

  urls <- vapply(seen(), function(r) r$url, "")
  expect_equal(sent_json(seen()[[1]]), list(tweet_id = "20"))
  expect_match(urls[2], "/retweets/20$")
  expect_match(urls[4], "/bookmarks/20$")
})

test_that("post ids must be strings of digits", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  expect_error(create_repost(20), "string of digits")
  expect_error(create_repost(), "`post_id` is missing")
  expect_error(hide_reply("abc"), "string of digits")
})

# create_post: plain arguments, list arguments and the cost line ------------

# Runs `expr` with messages captured, returning them.
capture_messages <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(expr, message = function(m) {
    msgs <<- c(msgs, trimws(conditionMessage(m)))
    invokeRestart("muffleMessage")
  })
  msgs
}

test_that("create_post turns the plain arguments into the API's objects", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  seen <- record_requests(function(req) {
    json_response(201, list(data = list(id = "1", text = "t")))
  })

  suppressMessages(create_post("photo", media_ids = c("11", "22")))
  suppressMessages(create_post("quote", quote_post_id = "33"))
  suppressMessages(create_post("reply", reply_to_post_id = "44"))
  suppressMessages(create_post("poll", poll_options = c("Yes", "No")))
  suppressMessages(create_post("poll", poll_options = c("A", "B", "C"),
                               poll_duration_minutes = 60))
  suppressMessages(create_post("community", community_id = "55",
                               paid_partnership = TRUE,
                               share_with_followers = TRUE,
                               reply_settings = "verified"))

  bodies <- lapply(seen(), sent_json)
  expect_equal(bodies[[1]], list(text = "photo", media = list(media_ids = list("11", "22"))))
  expect_equal(bodies[[2]], list(text = "quote", quote_tweet_id = "33"))
  expect_equal(bodies[[3]], list(text = "reply", reply = list(in_reply_to_tweet_id = "44")))
  expect_equal(bodies[[4]], list(
    text = "poll", poll = list(options = list("Yes", "No"), duration_minutes = 1440L)
  ))
  expect_equal(bodies[[5]]$poll$options, list("A", "B", "C"))
  expect_equal(bodies[[5]]$poll$duration_minutes, 60L)
  expect_equal(bodies[[6]], list(
    text = "community", paid_partnership = TRUE, share_with_followers = TRUE,
    community_id = "55", reply_settings = "verified"
  ))
  expect_true(all(vapply(seen(), function(r) grepl("/2/tweets$", r$url), TRUE)))
})

test_that("create_post refuses media with a poll and a part given twice", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")

  expect_error(create_post("x", media_ids = "1", poll_options = c("a", "b")),
               "both media and a poll")
  expect_error(create_post("x", media = list(media_ids = "1"),
                           poll = list(options = c("a", "b"), duration_minutes = 5)),
               "both media and a poll")
  expect_error(create_post("x", media_ids = "1", media = list(media_ids = "1")),
               "either `media_ids` or `media`")
  expect_error(create_post("x", poll_options = c("a", "b"),
                           poll = list(options = c("a", "b"), duration_minutes = 5)),
               "either `poll_options` or `poll`")
  expect_error(create_post("x", reply_to_post_id = "1",
                           reply = list(in_reply_to_tweet_id = "1")),
               "either `reply_to_post_id` or `reply`")
})

test_that("create_post checks ids, poll shape and reply_settings first", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")

  expect_error(create_post("x", media_ids = 11), "`media_ids` must be strings of digits")
  expect_error(create_post("x", media_ids = as.character(1:5)), "at most 4 ids")
  expect_error(create_post("x", quote_post_id = 33), "`quote_post_id` must be one string")
  expect_error(create_post("x", reply_to_post_id = "abc"), "`reply_to_post_id`")
  expect_error(create_post("x", community_id = "c1"), "`community_id`")
  expect_error(create_post("x", poll_options = "only one"), "2 to 4 strings")
  expect_error(create_post("x", poll_options = c("a", strrep("b", 26))), "25 characters")
  expect_error(create_post("x", poll_options = c("a", "b"), poll_duration_minutes = 2),
               "between 5 and 10,080")
  expect_error(create_post("x", reply_settings = "everyone"), "\"mentionedUsers\"")
  expect_error(create_post(c("a", "b")), "one string")
})

test_that("create_post announces the higher price when the text has a link", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  httr2::local_mocked_responses(function(req) {
    json_response(201, list(data = list(id = "1", text = "t")))
  })

  plain <- capture_messages(create_post("No link here, just words."))
  https <- capture_messages(create_post("Read this: https://example.com/post"))
  http  <- capture_messages(create_post("http://example.com"))
  www   <- capture_messages(create_post("See www.example.com for more"))

  expect_equal(plain, "This request costs about $0.015.")
  expect_equal(https, "This request costs about $0.200.")
  expect_equal(http,  "This request costs about $0.200.")
  expect_equal(www,   "This request costs about $0.200.")
})
