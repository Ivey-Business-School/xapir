#' Get Muting
#'
#' @description
#' Retrieves the users the signed-in user has muted via
#' the [get muting endpoint](https://docs.x.com/x-api/users/get-muting).
#' Needs a user token, so the first call opens a browser window to sign in.
#' Every user returned is billed, so the function says what the call can
#' cost before it reads anything, and stops reading at `max_users`.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr map pluck
#' @template user_fields
#' @param max_results \code{numeric}; the number of users per API call,
#'   between 10 and 100. The function stops before any request if the value
#'   is outside that range.
#' @param max_users \code{numeric}; the most users to read across all pages.
#'   Reading stops once this many have been returned. Default 500.
#' @template pagination_token
#' @return A tibble with one row per muted user and the 18 columns described
#'   in [extract_user()]: `created_at` (POSIXct, UTC), `username`, `name`,
#'   `description`, `followers_count`, `following_count`, `post_count`,
#'   `listed_count`, `like_count`, `protected`, `verified`, `verified_type`,
#'   `is_identity_verified`, `location`, `profile_image_url`, `link_in_bio`,
#'   `url` and `user_id`. When nobody is muted, the same columns with no
#'   rows.
#' @examples
#' \dontrun{
#' muted_users <- get_muting()
#' }
#' @export
get_muting <- function(
  user_fields      = default_user_fields(),
  max_results      = 100,
  max_users        = 500,
  pagination_token = NULL
) {
  check_max_results(max_results)
  check_max_users(max_users)
  announce_user_cap(max_users, cap_arg = "max_users")

  token <- authenticate_user()

  # The endpoint is addressed by the caller's own id.
  me <- x_request(token$access_token) |>
    req_url_path_append("users", "me") |>
    x_perform()
  user_id <- pluck(me, "data", "id")

  req <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "muting") |>
    req_url_query(user.fields = join_fields(user_fields))

  pages <- fetch_pages(
    req,
    max_posts        = max_users,
    max_results      = max_results,
    sleep_time       = 0,
    pagination_token = pagination_token,
    what             = "users"
  )

  pages |>
    map(~ pluck(.x, "data")) |>
    unlist(recursive = FALSE) |>
    users_table()
}
