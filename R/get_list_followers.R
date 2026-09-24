#' Get List Followers
#'
#' @description
#' Returns the users who follow a list via the
#' [get list followers endpoint](https://docs.x.com/x-api/lists/get-list-followers).
#' Every user returned is billed (US$0.010 each in September 2026), so the
#' function says what the call can cost before it reads anything, and stops
#' reading at `max_users`.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr map pluck
#' @importFrom dplyr relocate
#' @param list_id The list's id, as a string of digits.
#' @param max_results \code{numeric}; the number of users per API call,
#'   between 1 and 100. The function stops before any request if the value
#'   is outside that range.
#' @param max_users \code{numeric}; the most users to read across all pages.
#'   Reading stops once this many have been returned. Default 500.
#' @template pagination_token
#' @template sleep_time
#' @template bearer_token
#' @template user_fields
#' @return A tibble with one row per follower: `list_id`, then the 18
#'   columns described in [extract_user()]: `created_at` (POSIXct, UTC),
#'   `username`, `name`, `description`, `followers_count`,
#'   `following_count`, `post_count`, `listed_count`, `like_count`,
#'   `protected`, `verified`, `verified_type`, `is_identity_verified`,
#'   `location`, `profile_image_url`, `link_in_bio`, `url` and `user_id`. A
#'   list nobody follows gives the same columns with no rows.
#' @examples
#' \dontrun{
#' followers <- get_list_followers(list_id = "1146654567674912769")
#' }
#' @export
get_list_followers <- function(
  list_id,
  max_results      = 100,
  max_users        = 500,
  pagination_token = NULL,
  sleep_time       = 0,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  user_fields      = default_user_fields()
) {
  check_token(bearer_token)
  check_list_id(list_id)
  check_max_results(max_results, min = 1, max = 100, what = "users")
  check_max_users(max_users)
  announce_cap(max_users, what = "users", arg = "max_users")

  req <- x_request(bearer_token) |>
    req_url_path_append("lists", list_id, "followers") |>
    req_url_query(user.fields = join_fields(user_fields))

  pages <- fetch_pages(
    req,
    max_posts        = max_users,
    max_results      = max_results,
    sleep_time       = sleep_time,
    pagination_token = pagination_token,
    what             = "users",
    min_results      = 1
  )

  for (page in pages) {
    warn_partial_errors(page$errors, what = "users")
  }

  users <- pages |>
    map(~ pluck(.x, "data")) |>
    unlist(recursive = FALSE) |>
    users_table()

  users$list_id <- rep(list_id, nrow(users))
  relocate(users, "list_id")
}
