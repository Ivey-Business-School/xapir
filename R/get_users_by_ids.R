#' Get Users by IDs
#'
#' @description
#' Retrieves details of up to 100 users by their ids via
#' the [get users by IDs endpoint](https://docs.x.com/x-api/users/get-users-by-ids).
#' Every user returned is billed, so the function says what the call can
#' cost before it reads anything.
#'
#' An id the API cannot find does not stop the call: the users it did find
#' are returned, and one warning names each id that was not.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @param user_ids A character vector of up to 100 user ids, each a string
#'   of digits. Keep ids as text: as numbers they lose digits.
#' @template bearer_token
#' @template user_fields
#' @param expansions Not used by this endpoint. Accepted so that older code
#'   keeps running.
#' @return A tibble with one row per user and the 24 columns described in
#'   [extract_user()], from `created_at` to `user_id`. When no id is found,
#'   the same columns with no rows.
#' @examples
#' \dontrun{
#' users <- get_users_by_ids(c("783214", "2244994945"))
#' }
#' @export
get_users_by_ids <- function(
  user_ids,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  user_fields  = default_user_fields(),
  expansions   = NULL
) {
  check_token(bearer_token)
  user_ids <- check_user_ids(user_ids)
  announce_user_cap(length(user_ids))

  page <- x_request(bearer_token) |>
    req_url_path_append("users") |>
    req_url_query(
      ids         = str_c(user_ids, collapse = ","),
      user.fields = join_fields(user_fields)
    ) |>
    x_perform()

  warn_partial_errors(page$errors, what = "user ids")
  users_table(page$data)
}
