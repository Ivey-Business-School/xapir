#' Get Owned Lists
#'
#' @description
#' Retrieves the lists a user owns via the
#' [owned lists endpoint](https://docs.x.com/x-api/lists/get-a-users-owned-lists).
#'
#' Give either `username` or `user_id`, not both. A `username` costs one
#' user read to turn the handle into an id before the lists are read. When
#' you already know the account's id, pass `user_id` and that read is
#' skipped.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @template username
#' @param user_id \code{character}; the account's X user id, as a string of
#'   digits. When given, the handle lookup is skipped and `username` must be
#'   `NULL`.
#' @template bearer_token
#' @param list_fields \code{character}, \code{vector}; the fields to return
#'   for each list.
#' @return A tibble with one row per list: `list_id`, `list_name`,
#'   `description`, `created_at` (POSIXct, UTC), `follower_count`,
#'   `member_count`, `private` and `owner_id`. A user with no lists gives the
#'   same columns with no rows.
#' @examples
#' \dontrun{
#' lists <- get_owned_list(username = "Tesla")
#'
#' # The same lists by id, with no user read for the handle
#' lists <- get_owned_list(user_id = "13298072")
#' }
#' @export
get_owned_list <- function(
  username     = NULL,
  user_id      = NULL,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  list_fields  = c(
    "id", "name", "created_at", "description", "follower_count",
    "member_count", "private", "owner_id"
  )
) {
  check_token(bearer_token)
  check_one_of_user(username, user_id)

  if (is.null(user_id)) {
    user_id <- lookup_user_id(username, bearer_token)
  }

  page <- x_request(bearer_token) |>
    req_url_path_append("users", user_id, "owned_lists") |>
    req_url_query(list.fields = join_fields(list_fields)) |>
    x_perform()

  warn_partial_errors(page$errors, what = "lists")
  lists_table(page$data)
}
