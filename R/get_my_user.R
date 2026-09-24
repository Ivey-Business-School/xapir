#' Get My User
#'
#' @description
#' Returns details about the signed-in user via the
#' [get my user endpoint](https://docs.x.com/x-api/users/get-my-user).
#' Needs a user token, so the first call opens a browser window to sign in.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @template user_fields
#' @return A tibble with one row and the 24 columns described in
#'   [extract_user()], from `created_at` to `user_id`.
#' @examples
#' \dontrun{
#' my_user <- get_my_user()
#' }
#' @export
get_my_user <- function(
  user_fields = default_user_fields()
) {
  token <- authenticate_user()

  page <- x_request(token$access_token) |>
    req_url_path_append("users", "me") |>
    req_url_query(user.fields = join_fields(user_fields)) |>
    x_perform()

  warn_partial_errors(page$errors, what = "users")
  # This endpoint returns one user object in `data`, not a list of them.
  users_table(list(page$data))
}
