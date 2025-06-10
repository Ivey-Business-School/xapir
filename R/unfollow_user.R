#' Unfollow User
#'
#' @description
#' Causes the source User to unfollow the target User via the [unfollow user endpoint](https://docs.x.com/x-api/users/unfollow-user).
#' The source User must match the User context authorizing the request
#'
#' @importFrom httr2 request req_auth_bearer_token req_perform resp_body_json req_method
#' @param source_username Username of account that will unfollow someone.
#' @param target_username Username of account that will be unfollowed. 
#' @return A list with the API response that shows a successful action. 
#' @examples
#' \dontrun{
#' unfollow_user(source_username = "Tesla", target_username = "elonmusk")
#' }
#' @export
unfollow_user <- function(
  source_username, 
  target_username
) {

  # Get cached or refreshed token
  token <- authenticate_user()
  
  # Obtain the user_id of the source account
  source_url <- paste0("https://api.twitter.com/2/users/by/username/", source_username)
  
  source_req <- request(source_url) |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  
  source_resp <- resp_body_json(source_req)
  source_user_id <- source_resp$data$id

  # Obtain the user_id of the target account
  target_url <- paste0("https://api.twitter.com/2/users/by/username/", target_username)

  target_resp <- request(target_url) |>
    req_auth_bearer_token(token$access_token) |>
    req_perform() |>
    resp_body_json()

  target_user_id <- target_resp$data$id

  # Unfollow the user
  unfollow_url <- paste0("https://api.twitter.com/2/users/", source_user_id, "/following/", target_user_id)

  req <- request(unfollow_url) |>
    req_method("DELETE") |>
    req_auth_bearer_token(token$access_token)

  # Handle errors
  tryCatch(
    {
      resp <- req_perform(req)
      message("Unfollow successful!")
      resp_body_json(resp)
    },
    error = function(e) {
      if (!is.null(e$response) && e$response$status == 403) {
        err_detail <- resp_body_json(e$response)$detail
        if (grepl("not following", err_detail)) {
          message("Error: Source is not following target.")
        } else if (grepl("protected", err_detail)) {
          message("Error: Target account is protected.")
        } else {
          message("403 Forbidden. Check token permissions.")
        }
      } else {
        message("Error: ", e$message)
      }
      return(NULL)
    }
  )
}
