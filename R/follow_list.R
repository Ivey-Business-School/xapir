#' Follow List
#'
#' @description
#' Makes the signed-in account follow a list via the [follow list
#' endpoint](https://docs.x.com/x-api/users/follow-list). Needs a user
#' token, so the first call opens a browser window to sign in. The request
#' is billed, so the function says what it costs before it sends anything.
#'
#' @importFrom httr2 req_body_json req_method req_url_path_append
#' @param list_id The id of the list to follow, as a string.
#' @return Invisibly, the `data` list the API returns, `list(following =
#'   TRUE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' follow_list(list_id = "1146654567674912769")
#' }
#' @export
follow_list <- function(
  list_id
) {

  check_list_id(list_id)
  token   <- authenticate_user()
  user_id <- my_user_id(token)

  announce_request_cost("list_manage")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "followed_lists") |>
    req_method("POST") |>
    req_body_json(list(list_id = list_id)) |>
    x_perform()

  invisible(response$data)
}

#' Unfollow List
#'
#' @description
#' Makes the signed-in account unfollow a list via the [unfollow list
#' endpoint](https://docs.x.com/x-api/users/unfollow-list). Needs a user
#' token, so the first call opens a browser window to sign in. The request
#' is billed, so the function says what it costs before it sends anything.
#'
#' @importFrom httr2 req_method req_url_path_append
#' @param list_id The id of the list to unfollow, as a string.
#' @return Invisibly, the `data` list the API returns, `list(following =
#'   FALSE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' unfollow_list(list_id = "1146654567674912769")
#' }
#' @export
unfollow_list <- function(
  list_id
) {

  check_list_id(list_id)
  token   <- authenticate_user()
  user_id <- my_user_id(token)

  announce_request_cost("list_manage")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "followed_lists", list_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}
