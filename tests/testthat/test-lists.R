# A list object as the API sends it.
api_list <- function(id, name = paste("List", id)) {
  list(
    id = as.character(id), name = name, description = "about",
    created_at = "2021-05-06T07:08:09.000Z", follower_count = 3L,
    member_count = 12L, private = FALSE, owner_id = "42"
  )
}

# A member of a list, as the members endpoint sends one.
api_member <- function(id) {
  list(
    id = as.character(id), username = paste0("m", id), name = paste("M", id),
    created_at = "2020-01-02T03:04:05.000Z",
    public_metrics = list(followers_count = 1L, tweet_count = 2L)
  )
}

list_columns <- c(
  "list_id", "list_name", "description", "created_at", "follower_count",
  "member_count", "private", "owner_id"
)

not_found <- function(what) {
  json_response(200, list(errors = list(list(
    title = "Not Found Error",
    detail = paste0("Could not find ", what, ".")
  ))))
}

test_that("list_schema and lists_table agree, and list_row fills typed NAs", {
  expect_equal(names(list_schema()), list_columns)
  expect_identical(lists_table(list()), list_schema())
  expect_identical(lists_table(NULL), list_schema())
  row <- list_row(list())
  expect_equal(names(row), list_columns)
  expect_true(all(vapply(row, function(col) is.na(col), TRUE)))
  expect_s3_class(row$created_at, "POSIXct")
  expect_type(row$follower_count, "integer")
  expect_type(row$private, "logical")
})

test_that("get_owned_list on an unknown handle stops with the API's reason", {
  httr2::local_mocked_responses(list(not_found("user with username: [nobodyhere]")))
  expect_error(
    get_owned_list("nobodyhere", bearer_token = "tok"),
    "Could not find user with username"
  )
})

test_that("get_owned_list by username pays the lookup, then reads the lists", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("/users/by/username", req$url, fixed = TRUE)) {
      json_response(200, list(data = list(id = "42", username = "tesla")))
    } else {
      json_response(200, list(data = list(api_list(1), api_list(2))))
    }
  })
  lists <- get_owned_list("tesla", bearer_token = "tok")
  expect_equal(length(urls), 2)
  expect_match(urls[1], "/2/users/by/username/tesla", fixed = TRUE)
  expect_match(urls[2], "/2/users/42/owned_lists", fixed = TRUE)
  expect_equal(names(lists), list_columns)
  expect_equal(lists$list_id, c("1", "2"))
  expect_equal(lists$list_name, c("List 1", "List 2"))
  expect_s3_class(lists$created_at, "POSIXct")
  expect_equal(attr(lists$created_at, "tzone"), "UTC")
  expect_equal(lists$member_count, c(12L, 12L))
})

test_that("get_owned_list and get_followed_lists by user_id make no lookup", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(data = list(api_list(1))))
  })
  owned <- get_owned_list(user_id = "42", bearer_token = "tok")
  followed <- get_followed_lists(user_id = "42", bearer_token = "tok")
  expect_equal(length(urls), 2)
  expect_match(urls[1], "/2/users/42/owned_lists", fixed = TRUE)
  expect_match(urls[2], "/2/users/42/followed_lists", fixed = TRUE)
  expect_equal(nrow(owned), 1)
  expect_equal(names(followed), list_columns)
})

test_that("the list readers want exactly one of username and user_id", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_owned_list(bearer_token = "tok"), "either `username` or `user_id`")
  expect_error(
    get_followed_lists("tesla", user_id = "42", bearer_token = "tok"),
    "either `username` or `user_id`"
  )
  expect_error(get_owned_list(user_id = 42, bearer_token = "tok"), "string of digits")
  expect_error(get_followed_lists("tesla", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("a user with no lists gives the zero-row schema, silently", {
  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L))),
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  # one cost line each, no warning
  expect_message(owned <- get_owned_list(user_id = "42", bearer_token = "tok"),
                 "Reading up to 100 lists, about \\$0.50\\.")
  expect_identical(owned, list_schema())
  expect_message(followed <- get_followed_lists(user_id = "42", bearer_token = "tok"),
                 "Reading up to 100 lists, about \\$0.50\\.")
  expect_identical(followed, list_schema())
})

test_that("get_list_by_id returns one row in the list schema and stops when unknown", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("/lists/1146", req$url, fixed = TRUE)) {
      json_response(200, list(data = api_list("1146")))
    } else {
      not_found("list with id: [999]")
    }
  })
  lst <- get_list_by_id("1146", bearer_token = "tok")
  expect_match(urls[1], "/2/lists/1146?", fixed = TRUE)
  expect_equal(nrow(lst), 1)
  expect_equal(names(lst), list_columns)
  expect_equal(lst$list_id, "1146")
  expect_equal(lst$owner_id, "42")
  expect_error(get_list_by_id("999", bearer_token = "tok"), "Could not find list")
  expect_error(get_list_by_id(1146, bearer_token = "tok"), "string of digits")
})

test_that("get_list_member returns list_id plus the user columns", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(
      data = list(api_member(1), api_member(2)),
      meta = list(result_count = 2L)
    ))
  })
  msgs <- character(0)
  members <- withCallingHandlers(
    get_list_member("1146", bearer_token = "tok", max_users = 50),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_match(msgs[1], "Reading up to 50 users, about \\$0.50")
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/lists/1146/members", fixed = TRUE)
  expect_equal(names(members), c("list_id", names(user_schema())))
  expect_equal(members$list_id, c("1146", "1146"))
  expect_equal(members$user_id, c("1", "2"))
  expect_equal(members$post_count, c(2L, 2L))
  expect_type(members$is_identity_verified, "logical")
})

test_that("get_list_member pages and stops at max_users", {
  httr2::local_mocked_responses(function(req) {
    if (grepl("pagination_token=n2", req$url, fixed = TRUE)) {
      json_response(200, list(data = lapply(11:20, api_member),
                              meta = list(result_count = 10L)))
    } else {
      json_response(200, list(data = lapply(1:10, api_member),
                              meta = list(result_count = 10L, next_token = "n2")))
    }
  })
  members <- suppressMessages(
    get_list_member("1146", bearer_token = "tok", max_results = 10, max_users = 15)
  )
  expect_equal(nrow(members), 15)
  expect_equal(members$user_id, as.character(1:15))
})

test_that("get_list_member on an empty list is the zero-row schema with list_id", {
  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  members <- suppressMessages(get_list_member("1146", bearer_token = "tok"))
  expect_equal(nrow(members), 0)
  expect_equal(names(members), c("list_id", names(user_schema())))
  expect_type(members$list_id, "character")

  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_list_member(1146, bearer_token = "tok"), "string of digits")
  expect_error(get_list_member("1146", bearer_token = "tok", max_results = 5),
               "between 10 and 100")
})

test_that("get_trends_by_woeid returns the zero-row schema when data is empty", {
  httr2::local_mocked_responses(list(
    json_response(200, list(data = list()))
  ))
  expect_message(trends <- get_trends_by_woeid(4118, bearer_token = "tok"),
                 "costs about \\$0.010")
  expect_identical(trends, trend_schema())
  expect_equal(names(trends), c("trend_name", "post_count"))
  expect_type(trends$post_count, "integer")
})

test_that("get_trends_by_woeid reads trends and validates before any request", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(data = list(
      list(trend_name = "#Monday", tweet_count = 1234L),
      list(trend_name = "Toronto")
    )))
  })
  trends <- get_trends_by_woeid("4118", bearer_token = "tok", max_trends = 2)
  expect_match(urls[1], "/2/trends/by/woeid/4118?", fixed = TRUE)
  expect_match(urls[1], "max_trends=2", fixed = TRUE)
  expect_equal(trends$trend_name, c("#Monday", "Toronto"))
  expect_equal(trends$post_count, c(1234L, NA_integer_))

  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_trends_by_woeid("toronto", bearer_token = "tok"), "whole number")
  expect_error(get_trends_by_woeid(4118, max_trends = 51, bearer_token = "tok"),
               "between 1 and 50")
  expect_error(get_trends_by_woeid(4118, bearer_token = ""), "X_BEARER_TOKEN")
})
