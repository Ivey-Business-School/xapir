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

# A user object as the API sends it, with the fields the package reads. The
# same shape test-users.R builds; test files do not share definitions.
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
    url = url,
    entities = list(url = list(urls = list(list(display_url = "example.com")))),
    public_metrics = list(
      followers_count = 10L, following_count = 5L, tweet_count = 100L,
      listed_count = 2L, like_count = 7L
    )
  )
}

# A page of users with the given ids, and a next_token when given.
users_page <- function(ids, next_token = NULL) {
  meta <- list(result_count = length(ids))
  if (!is.null(next_token)) meta$next_token <- next_token
  json_response(200, list(data = lapply(ids, api_user), meta = meta))
}

user_columns <- names(user_schema())

# Followers and following ----------------------------------------------------

test_that("get_followers by username pays the lookup, hits /followers and announces $10", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("/users/by/username", req$url, fixed = TRUE)) {
      json_response(200, list(data = list(id = "42", username = "tesla")))
    } else {
      users_page(1:3)
    }
  })
  out <- collect_messages(get_followers("tesla", bearer_token = "tok"))
  expect_match(out$msgs[1], "Reading up to 1,000 users, about \\$10\\.00")
  expect_match(out$msgs[1], "Set max_users to change this")
  expect_equal(length(urls), 2)
  expect_match(urls[1], "/2/users/by/username/tesla", fixed = TRUE)
  expect_match(urls[2], "/2/users/42/followers?", fixed = TRUE)
  expect_match(urls[2], "max_results=1000", fixed = TRUE)
  expect_match(urls[2], "user.fields=created_at", fixed = TRUE)
  expect_equal(nrow(out$result), 3)
  expect_equal(out$result$user_id, c("1", "2", "3"))
  expect_equal(names(out$result), user_columns)
  expect_type(out$result$user_id, "character")
})

test_that("get_following by user_id makes no lookup and hits /following", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    users_page(1:2)
  })
  following <- suppressMessages(
    get_following(user_id = "42", max_users = 50, bearer_token = "tok")
  )
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/users/42/following?", fixed = TRUE)
  # the page asks for no more than the cap
  expect_match(urls[1], "max_results=50", fixed = TRUE)
  expect_equal(following$user_id, c("1", "2"))
})

test_that("get_followers pages twice and returns every row, then stops at max_users", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("pagination_token=n2", req$url, fixed = TRUE)) {
      users_page(3:4)
    } else {
      users_page(1:2, next_token = "n2")
    }
  })
  out <- collect_messages(get_followers(user_id = "42", bearer_token = "tok"))
  expect_equal(length(urls), 2)
  expect_equal(nrow(out$result), 4)
  expect_equal(out$result$user_id, c("1", "2", "3", "4"))
  expect_equal(out$msgs[2], "Finished getting users on page 1")
  expect_equal(out$msgs[3], "Finished getting users on page 2")
  expect_equal(out$msgs[4], "Read 4 users, about $0.04.")

  # a cap below the first page trims it and asks for no second page
  httr2::local_mocked_responses(function(req) users_page(1:20, next_token = "more"))
  capped <- suppressMessages(
    get_followers(user_id = "42", max_users = 15, bearer_token = "tok")
  )
  expect_equal(nrow(capped), 15)
})

test_that("get_followers and get_following validate before any request", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(
    get_followers("tesla", max_results = 1001, bearer_token = "tok"),
    "between 1 and 1000"
  )
  expect_error(
    get_following("tesla", max_results = 0, bearer_token = "tok"),
    "between 1 and 1000"
  )
  expect_error(
    get_followers("tesla", max_users = Inf, bearer_token = "tok"),
    "`max_users` must be a finite number"
  )
  expect_error(get_followers("tesla", "42", bearer_token = "tok"), "not both")
  expect_error(get_following(bearer_token = "tok"), "not neither")
  expect_error(get_followers(user_id = 42, bearer_token = "tok"), "as text")
  expect_error(get_followers("tesla", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("get_followers with nobody following is the zero-row schema", {
  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  followers <- suppressMessages(get_followers(user_id = "42", bearer_token = "tok"))
  expect_identical(followers, user_schema())
})

test_that("get_followers warns once about partial errors and keeps the rows", {
  httr2::local_mocked_responses(list(
    json_response(200, list(
      data = list(api_user(1)),
      errors = list(list(title = "Forbidden", detail = "User is suspended.")),
      meta = list(result_count = 1L)
    ))
  ))
  expect_warning(
    followers <- suppressMessages(get_followers(user_id = "42", bearer_token = "tok")),
    "suspended"
  )
  expect_equal(nrow(followers), 1)
})

# Reposted by ----------------------------------------------------------------

test_that("get_reposted_by hits /retweeted_by, announces the user price and pages", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("pagination_token=n2", req$url, fixed = TRUE)) {
      users_page(3)
    } else {
      users_page(1:2, next_token = "n2")
    }
  })
  out <- collect_messages(get_reposted_by("20", bearer_token = "tok"))
  expect_match(out$msgs[1], "Reading up to 100 users, about \\$1\\.00")
  expect_match(out$msgs[1], "Set max_users to change this")
  expect_equal(length(urls), 2)
  expect_match(urls[1], "/2/tweets/20/retweeted_by?", fixed = TRUE)
  expect_match(urls[1], "max_results=100", fixed = TRUE)
  expect_equal(nrow(out$result), 3)
  expect_equal(out$result$user_id, c("1", "2", "3"))
  expect_equal(names(out$result), user_columns)
})

test_that("get_reposted_by validates before any request and handles no data", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_reposted_by("20", max_results = 101, bearer_token = "tok"), "between 1 and 100")
  expect_error(get_reposted_by("20", max_results = 0, bearer_token = "tok"), "between 1 and 100")
  expect_error(get_reposted_by(20, bearer_token = "tok"), "as text")
  expect_error(get_reposted_by("20", max_users = 0, bearer_token = "tok"), "1 or more")
  expect_error(get_reposted_by("20", bearer_token = ""), "X_BEARER_TOKEN")

  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  reposters <- suppressMessages(get_reposted_by("20", bearer_token = "tok"))
  expect_identical(reposters, user_schema())
})
