#' Unmute User
#'
#' @description
#' Makes the source account unmute the target account via the [unmute user
#' endpoint](https://docs.x.com/x-api/users/unmute-user). The source must be
#' the account that signed in. Needs a user token, so the first call opens a
#' browser window to sign in.
#'
#' @importFrom httr2 req_method
#' @param source_username Username of the account that will unmute, without
#'   the "@" symbol. Must be the account that signed in.
#' @param target_username Username of the account to unmute, without the "@"
#'   symbol.
#' @return Invisibly, the `data` list the API returns, `list(muting = FALSE)`.
#'   Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' unmute_user(source_username = "myaccount", target_username = "noisyaccount")
#' }
#' @export
unmute_user <- function(
  source_username,
  target_username
) {

  token          <- authenticate_user()
  source_user_id <- lookup_user_id(source_username, token$access_token)
  target_user_id <- lookup_user_id(target_username, token$access_token)

  announce_request_cost("interaction_delete")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", source_user_id, "muting", target_user_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}
