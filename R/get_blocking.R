#' Get Blocking
#'
#' @description
#' Retrieves the users the signed-in user has blocked via
#' the [get blocking endpoint](https://docs.x.com/x-api/users/get-blocking).
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
#' @return A tibble with one row per blocked user and the 24 columns
#'   described in [extract_user()], from `created_at` to `user_id`. When
#'   nobody is blocked, the same columns with no rows.
#' @examples
#' \dontrun{
#' blocked_users <- get_blocking()
#' }
#' @export
get_blocking <- function(
  user_fields      = default_user_fields(),
  max_results      = 100,
  max_users        = 500,
  pagination_token = NULL
) {
  check_max_results(max_results)
  check_max_users(max_users)
  announce_user_cap(max_users, cap_arg = "max_users", what = "blocks", owned = TRUE)

  token <- authenticate_user()

  # The endpoint is addressed by the caller's own id.
  user_id <- my_user_id(token)

  req <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "blocking") |>
    req_url_query(user.fields = join_fields(user_fields))

  pages <- fetch_pages(
    req,
    max_posts        = max_users,
    max_results      = max_results,
    sleep_time       = 0,
    pagination_token = pagination_token,
    what             = "blocks",
    owned            = TRUE
  )

  pages |>
    map(~ pluck(.x, "data")) |>
    unlist(recursive = FALSE) |>
    users_table()
}
