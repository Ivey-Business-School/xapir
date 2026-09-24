# Every test here mocks the API. Nothing calls X.

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

user_columns <- names(user_schema())

# A list object as the API sends it.
api_list <- function(id, name = paste("List", id)) {
  list(
    id = as.character(id), name = name, description = "about",
    created_at = "2021-05-06T07:08:09.000Z", follower_count = 3L,
    member_count = 12L, private = FALSE, owner_id = "42"
  )
}

# A page of lists with the given ids, and a next_token when given.
lists_page <- function(ids, next_token = NULL) {
  meta <- list(result_count = length(ids))
  if (!is.null(next_token)) meta$next_token <- next_token
  json_response(200, list(data = lapply(ids, api_list), meta = meta))
}

list_columns <- c(
  "list_id", "list_name", "description", "created_at", "follower_count",
  "member_count", "private", "owner_id"
)

# List posts -----------------------------------------------------------------

test_that("get_list_posts hits /lists/{id}/tweets with the post fields and returns pages", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("pagination_token=n2", req$url, fixed = TRUE)) {
      posts_page(101:150)
    } else {
      posts_page(1:100, next_token = "n2")
    }
  })
  out <- collect_messages(get_list_posts("123", bearer_token = "tok"))
  expect_match(out$msgs[1], "Reading up to 500 posts, about \\$2\\.50")
  expect_equal(length(urls), 2)
  expect_match(urls[1], "/2/lists/123/tweets?", fixed = TRUE)
  expect_match(urls[1], "tweet.fields=created_at", fixed = TRUE)
  expect_match(urls[1], "expansions=author_id", fixed = TRUE)
  expect_match(urls[1], "max_results=100", fixed = TRUE)
  expect_match(urls[2], "pagination_token=n2", fixed = TRUE)
  pages <- out$result
  expect_type(pages, "list")
  expect_equal(length(pages), 2)
  expect_equal(sum(vapply(pages, function(p) length(p$data), 1L)), 150)
  # the pages feed the extractors like a timeline does
  posts <- extract_post(pages)
  expect_equal(nrow(posts), 150)
})

test_that("get_list_posts never holds more than max_posts and validates first", {
  httr2::local_mocked_responses(function(req) posts_page(1:100, next_token = "more"))
  pages <- suppressMessages(get_list_posts("123", max_posts = 120, bearer_token = "tok"))
  expect_equal(sum(vapply(pages, function(p) length(p$data), 1L)), 120)

  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_list_posts("123", max_results = 101, bearer_token = "tok"), "between 1 and 100")
  expect_error(get_list_posts("123", max_results = 0, bearer_token = "tok"), "between 1 and 100")
  expect_error(get_list_posts("123", max_posts = Inf, bearer_token = "tok"), "finite number")
  expect_error(get_list_posts(123, bearer_token = "tok"), "`list_id` must be one string of digits")
  expect_error(get_list_posts("123", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("get_list_posts on an empty list gives one page with no data", {
  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  pages <- suppressMessages(get_list_posts("123", bearer_token = "tok"))
  expect_equal(length(pages), 1)
  expect_equal(length(pages[[1]]$data), 0)
  expect_equal(nrow(extract_post(pages)), 0)
})

# List followers -------------------------------------------------------------

test_that("get_list_followers returns list_id plus the user columns and pages", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("pagination_token=n2", req$url, fixed = TRUE)) {
      users_page(3:4)
    } else {
      users_page(1:2, next_token = "n2")
    }
  })
  out <- collect_messages(get_list_followers("123", bearer_token = "tok"))
  expect_match(out$msgs[1], "Reading up to 500 users, about \\$5\\.00")
  expect_equal(length(urls), 2)
  expect_match(urls[1], "/2/lists/123/followers?", fixed = TRUE)
  expect_match(urls[1], "user.fields=created_at", fixed = TRUE)
  followers <- out$result
  expect_equal(names(followers), c("list_id", user_columns))
  expect_equal(nrow(followers), 4)
  expect_equal(followers$list_id, rep("123", 4))
  expect_equal(followers$user_id, c("1", "2", "3", "4"))
})

test_that("get_list_followers stops at max_users, validates first, and handles no data", {
  httr2::local_mocked_responses(function(req) users_page(1:20, next_token = "more"))
  followers <- suppressMessages(get_list_followers("123", max_users = 7, bearer_token = "tok"))
  expect_equal(nrow(followers), 7)

  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_list_followers("123", max_results = 101, bearer_token = "tok"), "between 1 and 100")
  expect_error(get_list_followers("123", max_users = 0, bearer_token = "tok"), "1 or more")
  expect_error(get_list_followers("abc", bearer_token = "tok"), "`list_id`")

  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  empty <- suppressMessages(get_list_followers("123", bearer_token = "tok"))
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), c("list_id", user_columns))
  expect_type(empty$list_id, "character")
})

# List memberships -----------------------------------------------------------

test_that("get_list_memberships by username pays the lookup and reads the lists", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("/users/by/username", req$url, fixed = TRUE)) {
      json_response(200, list(data = list(id = "42", username = "tesla")))
    } else if (grepl("pagination_token=n2", req$url, fixed = TRUE)) {
      lists_page(3)
    } else {
      lists_page(1:2, next_token = "n2")
    }
  })
  out <- collect_messages(get_list_memberships("tesla", bearer_token = "tok"))
  # lists are $0.005 each and the cap is named after lists
  expect_match(out$msgs[1], "Reading up to 100 lists, about \\$0\\.50")
  expect_match(out$msgs[1], "Set max_lists to change this")
  expect_equal(length(urls), 3)
  expect_match(urls[1], "/2/users/by/username/tesla", fixed = TRUE)
  expect_match(urls[2], "/2/users/42/list_memberships?", fixed = TRUE)
  expect_match(urls[2], "list.fields=id%2Cname", fixed = TRUE)
  expect_match(urls[3], "pagination_token=n2", fixed = TRUE)
  lists <- out$result
  expect_equal(names(lists), list_columns)
  expect_equal(lists$list_id, c("1", "2", "3"))
  expect_equal(attr(lists$created_at, "tzone"), "UTC")
})

test_that("get_list_memberships by user_id makes no lookup, stops at max_lists, validates", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    lists_page(1:20, next_token = "more")
  })
  lists <- suppressMessages(
    get_list_memberships(user_id = "42", max_lists = 5, bearer_token = "tok")
  )
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/users/42/list_memberships?", fixed = TRUE)
  expect_match(urls[1], "max_results=5", fixed = TRUE)
  expect_equal(nrow(lists), 5)

  httr2::local_mocked_responses(function(req) stop("a request was made"))
  expect_error(get_list_memberships("tesla", max_results = 101, bearer_token = "tok"), "between 1 and 100")
  expect_error(get_list_memberships("tesla", max_lists = Inf, bearer_token = "tok"), "`max_lists` must be a finite number")
  expect_error(get_list_memberships("tesla", "42", bearer_token = "tok"), "not both")
  expect_error(get_list_memberships(bearer_token = "tok"), "not neither")
})

test_that("get_list_memberships on a user in no lists is the zero-row schema", {
  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  lists <- suppressMessages(get_list_memberships(user_id = "42", bearer_token = "tok"))
  expect_identical(lists, list_schema())
})
