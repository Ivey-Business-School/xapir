#' Get Users by Usernames
#'
#' @description
#' Retrieves details of up to 100 users by their usernames via
#' the [get users by usernames endpoint](https://docs.x.com/x-api/users/get-users-by-usernames).
#' Every user returned is billed, so the function says what the call can
#' cost before it reads anything.
#'
#' A handle the API cannot find does not stop the call: the users it did
#' find are returned, and one warning names each handle that was not.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @param usernames A character vector of up to 100 handles, with or
#'   without the leading "@".
#' @template bearer_token
#' @template user_fields
#' @param expansions Not used by this endpoint. Accepted so that older code
#'   keeps running.
#' @return A tibble with one row per user and the 24 columns described in
#'   [extract_user()], from `created_at` to `user_id`. When no handle is
#'   found, the same columns with no rows.
#' @examples
#' \dontrun{
#' users <- get_users_by_usernames(c("Tesla", "XDevelopers"))
#' }
#' @export
get_users_by_usernames <- function(
  usernames,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  user_fields  = default_user_fields(),
  expansions   = NULL
) {
  check_token(bearer_token)
  usernames <- check_usernames(usernames)
  announce_user_cap(length(usernames))

  page <- x_request(bearer_token) |>
    req_url_path_append("users", "by") |>
    req_url_query(
      usernames   = str_c(usernames, collapse = ","),
      user.fields = join_fields(user_fields)
    ) |>
    x_perform()

  warn_partial_errors(page$errors, what = "usernames")
  users_table(page$data)
}
