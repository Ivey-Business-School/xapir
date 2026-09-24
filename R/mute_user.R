#' Mute User
#'
#' @description
#' Makes the source account mute the target account via the [mute user
#' endpoint](https://docs.x.com/x-api/users/mute-user). The source must be the
#' account that signed in. Needs a user token, so the first call opens a
#' browser window to sign in.
#'
#' @importFrom httr2 req_body_json req_method
#' @param source_username Username of the account that will mute, without the
#'   "@" symbol. Must be the account that signed in.
#' @param target_username Username of the account to mute, without the "@"
#'   symbol.
#' @return Invisibly, the `data` list the API returns, `list(muting = TRUE)`.
#'   Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' mute_user(source_username = "myaccount", target_username = "noisyaccount")
#' }
#' @export
mute_user <- function(
  source_username,
  target_username
) {

  token          <- authenticate_user()
  source_user_id <- lookup_user_id(source_username, token$access_token)
  target_user_id <- lookup_user_id(target_username, token$access_token)

  response <- x_request(token$access_token) |>
    req_url_path_append("users", source_user_id, "muting") |>
    req_method("POST") |>
    req_body_json(list(target_user_id = target_user_id)) |>
    x_perform()

  invisible(response$data)
}
