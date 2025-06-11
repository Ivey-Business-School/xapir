#' Follow User
#'
#' @description
#' Causes the User(in the path) to follow, or “request to follow” for protected Users, the target User
#' via the [follow user endpoint](https://docs.x.com/x-api/users/follow-user).
#' The User(in the path) must match the User context authorizing the request
#'
#' @importFrom httr2 request req_auth_bearer_token req_perform resp_body_json
#' @param source_username Username of account that will follow someone.
#' @param target_username Username of account that will be followed. 
#' @examples
#' \dontrun{
#' follow_user(source_username = "Tesla", target_username = "elonmusk")
#' }
#' @export
follow_user <- function(
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

  url <- paste0("https://api.twitter.com/2/users/", source_user_id, "/following")

  req <- request(url) |>
    req_auth_bearer_token(token$access_token) |>
    req_body_json(list(target_user_id = target_user_id)) |>
    req_perform()

  resp <- resp_body_json(req)
}
