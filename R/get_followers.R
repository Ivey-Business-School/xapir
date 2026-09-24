#' Get Followers
#'
#' @description
#' Returns the users who follow an account via the
#' [get followers endpoint](https://docs.x.com/x-api/users/get-followers).
#' Every user returned is billed (US$0.010 each in September 2026), so the
#' function says what the call can cost before it reads anything, and stops
#' reading at `max_users`. The default `max_users = 1000` is about US$10.00;
#' lower it for a first look at a big account.
#'
#' Give either `username` or `user_id`, not both. A `username` costs one
#' user read to turn the handle into an id before the followers are read.
#' When you already know the account's id, pass `user_id` and that read is
#' skipped.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr map pluck
#' @template username
#' @param user_id \code{character}; the account's X user id, as a string of
#'   digits. When given, the handle lookup is skipped and `username` must be
#'   `NULL`.
#' @param max_results \code{numeric}; the number of users per API call,
#'   between 1 and 1,000. The function stops before any request if the value
#'   is outside that range.
#' @param max_users \code{numeric}; the most users to read across all pages.
#'   Reading stops once this many have been returned. Default 1,000, about
#'   US$10.00.
#' @template pagination_token
#' @template sleep_time
#' @template bearer_token
#' @template user_fields
#' @return A tibble with one row per follower and the 18 columns described
#'   in [extract_user()]: `created_at` (POSIXct, UTC), `username`, `name`,
#'   `description`, `followers_count`, `following_count`, `post_count`,
#'   `listed_count`, `like_count`, `protected`, `verified`, `verified_type`,
#'   `is_identity_verified`, `location`, `profile_image_url`, `link_in_bio`,
#'   `url` and `user_id`. An account with no followers gives the same
#'   columns with no rows.
#' @examples
#' \dontrun{
#' followers <- get_followers("XDevelopers", max_users = 200)
#'
#' # The same followers by id, with no user read for the handle
#' followers <- get_followers(user_id = "2244994945", max_users = 200)
#' }
#' @export
get_followers <- function(
  username         = NULL,
  user_id          = NULL,
  max_results      = 1000,
  max_users        = 1000,
  pagination_token = NULL,
  sleep_time       = 0,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  user_fields      = default_user_fields()
) {
  get_follows(
    "followers", username, user_id, max_results, max_users,
    pagination_token, sleep_time, bearer_token, user_fields
  )
}

#' Get Following
#'
#' @description
#' Returns the users an account follows via the
#' [get following endpoint](https://docs.x.com/x-api/users/get-following).
#' Every user returned is billed (US$0.010 each in September 2026), so the
#' function says what the call can cost before it reads anything, and stops
#' reading at `max_users`. The default `max_users = 1000` is about US$10.00.
#'
#' Give either `username` or `user_id`, not both. A `username` costs one
#' user read to turn the handle into an id before the accounts are read.
#' When you already know the account's id, pass `user_id` and that read is
#' skipped.
#'
#' @inheritParams get_followers
#' @return A tibble with one row per followed account and the 18 columns
#'   described in [extract_user()]: `created_at` (POSIXct, UTC), `username`,
#'   `name`, `description`, `followers_count`, `following_count`,
#'   `post_count`, `listed_count`, `like_count`, `protected`, `verified`,
#'   `verified_type`, `is_identity_verified`, `location`,
#'   `profile_image_url`, `link_in_bio`, `url` and `user_id`. An account that
#'   follows nobody gives the same columns with no rows.
#' @examples
#' \dontrun{
#' following <- get_following("XDevelopers", max_users = 200)
#'
#' # The same accounts by id, with no user read for the handle
#' following <- get_following(user_id = "2244994945", max_users = 200)
#' }
#' @export
get_following <- function(
  username         = NULL,
  user_id          = NULL,
  max_results      = 1000,
  max_users        = 1000,
  pagination_token = NULL,
  sleep_time       = 0,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  user_fields      = default_user_fields()
) {
  get_follows(
    "following", username, user_id, max_results, max_users,
    pagination_token, sleep_time, bearer_token, user_fields
  )
}

# Both directions of the follow graph share one endpoint shape:
# /2/users/{id}/<followers|following>, 1 to 1,000 users a page, billed per
# user at the follows price.
get_follows <- function(direction, username, user_id, max_results, max_users,
                        pagination_token, sleep_time, bearer_token,
                        user_fields) {
  check_token(bearer_token)
  check_one_of_user(username, user_id)
  check_max_results(max_results, min = 1, max = 1000, what = "users")
  check_max_users(max_users)
  announce_cap(max_users, what = "follows", arg = "max_users")

  # A handle costs a user read to resolve; an id addresses the endpoint
  # directly and costs nothing extra.
  if (is.null(user_id)) {
    user_id <- lookup_user_id(username, bearer_token)
  }

  req <- x_request(bearer_token) |>
    req_url_path_append("users", user_id, direction) |>
    req_url_query(user.fields = join_fields(user_fields))

  pages <- fetch_pages(
    req,
    max_posts        = max_users,
    max_results      = max_results,
    sleep_time       = sleep_time,
    pagination_token = pagination_token,
    what             = "follows",
    min_results      = 1
  )

  for (page in pages) {
    warn_partial_errors(page$errors, what = "users")
  }

  pages |>
    map(~ pluck(.x, "data")) |>
    unlist(recursive = FALSE) |>
    users_table()
}
