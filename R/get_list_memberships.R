#' Get List Memberships
#'
#' @description
#' Returns the lists a user has been added to via the
#' [get list memberships endpoint](https://docs.x.com/x-api/users/get-list-memberships).
#' Every list returned is billed (US$0.005 each in September 2026), so the
#' function says what the call can cost before it reads anything, and stops
#' reading at `max_lists`.
#'
#' Give either `username` or `user_id`, not both. A `username` costs one
#' user read to turn the handle into an id before the lists are read. When
#' you already know the account's id, pass `user_id` and that read is
#' skipped.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr map pluck
#' @template username
#' @param user_id \code{character}; the account's X user id, as a string of
#'   digits. When given, the handle lookup is skipped and `username` must be
#'   `NULL`.
#' @param max_results \code{numeric}; the number of lists per API call,
#'   between 1 and 100. The function stops before any request if the value
#'   is outside that range.
#' @param max_lists \code{numeric}; the most lists to read across all pages.
#'   Reading stops once this many have been returned. Default 100.
#' @template pagination_token
#' @template sleep_time
#' @template bearer_token
#' @param list_fields \code{character}, \code{vector}; the fields to return
#'   for each list.
#' @return A tibble with one row per list: `list_id`, `list_name`,
#'   `description`, `created_at` (POSIXct, UTC), `follower_count`,
#'   `member_count`, `private` and `owner_id`. A user on no lists gives the
#'   same columns with no rows.
#' @examples
#' \dontrun{
#' lists <- get_list_memberships(username = "XDevelopers")
#'
#' # The same lists by id, with no user read for the handle
#' lists <- get_list_memberships(user_id = "2244994945")
#' }
#' @export
get_list_memberships <- function(
  username         = NULL,
  user_id          = NULL,
  max_results      = 100,
  max_lists        = 100,
  pagination_token = NULL,
  sleep_time       = 0,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  list_fields      = c(
    "id", "name", "created_at", "description", "follower_count",
    "member_count", "private", "owner_id"
  )
) {
  check_token(bearer_token)
  check_one_of_user(username, user_id)
  check_max_results(max_results, min = 1, max = 100, what = "lists")
  check_max_posts(max_lists, arg = "max_lists")
  announce_cap(max_lists, what = "lists", arg = "max_lists")

  if (is.null(user_id)) {
    user_id <- lookup_user_id(username, bearer_token)
  }

  req <- x_request(bearer_token) |>
    req_url_path_append("users", user_id, "list_memberships") |>
    req_url_query(list.fields = join_fields(list_fields))

  pages <- fetch_pages(
    req,
    max_posts        = max_lists,
    max_results      = max_results,
    sleep_time       = sleep_time,
    pagination_token = pagination_token,
    what             = "lists",
    min_results      = 1
  )

  for (page in pages) {
    warn_partial_errors(page$errors, what = "lists")
  }

  pages |>
    map(~ pluck(.x, "data")) |>
    unlist(recursive = FALSE) |>
    lists_table()
}
