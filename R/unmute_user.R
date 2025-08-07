#' Unmute User on X
#'
#' @description
#' Causes the authenticated user to unmute a specific user by their ID via 
#' the [unmute user endpoint](https://docs.x.com/x-api/users/unmute-user).
#'
#' @importFrom httr2 request req_auth_bearer_token req_body_json req_method req_perform resp_body_json
#' @param source_username Username of account that will unmute someone.
#' @param target_username Username of account that will be unmuted.
#' @examples
#' \dontrun{
#' unmute_user(source_username = "myaccount", target_username = "username_to_mute")
#' }
#' @export
unmute_user <- function(
  source_username,
  target_username
) {
  # Get cached or refreshed token
  token <- authenticate_user()
  
  # Obtain the user_id of the source account
  url <- paste0("https://api.twitter.com/2/users/by/username/", source_username)
  req <- request(url) |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  resp <- resp_body_json(req)
  source_user_id <- resp$data$id
  
  # Obtain the user_id of the target account
  url <- paste0("https://api.twitter.com/2/users/by/username/", target_username)
  req <- request(url) |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  resp <- resp_body_json(req)
  target_user_id <- resp$data$id
  
  # Mute the target user
  url <- paste0("https://api.twitter.com/2/users/", source_user_id, "/muting/", target_user_id)
  req <- request(url) |>
    req_method("DELETE") |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  
  resp <- resp_body_json(req)
}