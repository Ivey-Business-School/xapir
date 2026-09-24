#' Get List Members
#'
#' @description
#' Returns the users that are members of a list via the
#' [get list members endpoint](https://docs.x.com/x-api/users/returns-user-objects-that-are-members-of-a-list-by-the-provided-list-id).
#' Every user returned is billed, so the function says what the call can
#' cost before it reads anything, and stops reading at `max_users`.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr map pluck
#' @importFrom dplyr relocate
#' @param list_id The list's id, as a string of digits.
#' @template bearer_token
#' @template user_fields
#' @param max_results \code{numeric}; the number of users per API call,
#'   between 10 and 100. The function stops before any request if the value
#'   is outside that range.
#' @param max_users \code{numeric}; the most users to read across all pages.
#'   Reading stops once this many have been returned. Default 500.
#' @template pagination_token
#' @return A tibble with one row per member: `list_id`, then the 24 columns
#'   described in [extract_user()], from `created_at` to `user_id`. A list
#'   with no members gives the same columns with no rows.
#' @examples
#' \dontrun{
#' members <- get_list_member(list_id = "1146654567674912769")
#' }
#' @export
get_list_member <- function(
  list_id,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  user_fields      = default_user_fields(),
  max_results      = 100,
  max_users        = 500,
  pagination_token = NULL
) {
  check_token(bearer_token)
  check_list_id(list_id)
  check_max_results(max_results)
  check_max_users(max_users)
  announce_user_cap(max_users, cap_arg = "max_users")

  req <- x_request(bearer_token) |>
    req_url_path_append("lists", list_id, "members") |>
    req_url_query(user.fields = join_fields(user_fields))

  pages <- fetch_pages(
    req,
    max_posts        = max_users,
    max_results      = max_results,
    sleep_time       = 0,
    pagination_token = pagination_token,
    what             = "users"
  )

  users <- pages |>
    map(~ pluck(.x, "data")) |>
    unlist(recursive = FALSE) |>
    users_table()

  users$list_id <- rep(list_id, nrow(users))
  relocate(users, "list_id")
}
