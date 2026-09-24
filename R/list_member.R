#' Add List Member
#'
#' @description
#' Adds an account to a list the signed-in account owns via the [add list
#' member endpoint](https://docs.x.com/x-api/lists/add-list-member). Needs a
#' user token, so the first call opens a browser window to sign in. The
#' request is billed, so the function says what it costs before it sends
#' anything.
#'
#' Give either `username` or `user_id`, not both. A handle costs one user
#' read to turn it into an id before the member is added; pass the id when
#' you already know it and that read is skipped.
#'
#' @importFrom httr2 req_body_json req_method req_url_path_append
#' @param list_id The id of the list, as a string.
#' @param username Username of the account to add, without the "@" symbol.
#' @param user_id The id of the account to add, as a string of digits. When
#'   given, `username` must be `NULL`.
#' @return Invisibly, the `data` list the API returns, `list(is_member =
#'   TRUE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' add_list_member(list_id = "1146654567674912769", username = "Tesla")
#' add_list_member(list_id = "1146654567674912769", user_id = "13298072")
#' }
#' @export
add_list_member <- function(
  list_id,
  username = NULL,
  user_id  = NULL
) {

  check_list_id(list_id)
  check_one_of_user(username, user_id)
  token   <- authenticate_user()
  user_id <- user_id %||% lookup_user_id(sub("^@", "", username), token$access_token)

  announce_request_cost("list_manage")

  response <- x_request(token$access_token) |>
    req_url_path_append("lists", list_id, "members") |>
    req_method("POST") |>
    req_body_json(list(user_id = user_id)) |>
    x_perform()

  invisible(response$data)
}

#' Remove List Member
#'
#' @description
#' Removes an account from a list the signed-in account owns via the [remove
#' list member endpoint](https://docs.x.com/x-api/lists/remove-list-member).
#' Needs a user token, so the first call opens a browser window to sign in.
#' The request is billed, so the function says what it costs before it sends
#' anything.
#'
#' Give either `username` or `user_id`, not both. A handle costs one user
#' read to turn it into an id before the member is removed.
#'
#' @importFrom httr2 req_method req_url_path_append
#' @param list_id The id of the list, as a string.
#' @param username Username of the account to remove, without the "@"
#'   symbol.
#' @param user_id The id of the account to remove, as a string of digits.
#'   When given, `username` must be `NULL`.
#' @return Invisibly, the `data` list the API returns, `list(is_member =
#'   FALSE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' remove_list_member(list_id = "1146654567674912769", username = "Tesla")
#' }
#' @export
remove_list_member <- function(
  list_id,
  username = NULL,
  user_id  = NULL
) {

  check_list_id(list_id)
  check_one_of_user(username, user_id)
  token   <- authenticate_user()
  user_id <- user_id %||% lookup_user_id(sub("^@", "", username), token$access_token)

  announce_request_cost("list_manage")

  response <- x_request(token$access_token) |>
    req_url_path_append("lists", list_id, "members", user_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}
