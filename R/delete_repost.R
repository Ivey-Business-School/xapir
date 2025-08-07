#' Unrepost a Post on X
#'
#' @description
#' Causes the authenticated user to repost a specific Post by its ID. The User in the path must match the User 
#' context authorizing the request. This is done via the [retweet endpoint](https://docs.x.com/x-api/posts/unrepost-post).
#' 
#' @importFrom httr2 request req_auth_bearer_token req_method req_perform resp_body_json
#' @param tweet_id The ID of the post to be unreposted.
#' @examples
#' \dontrun{
#' delete_repost(tweet_id = "20")
#' }
#' @export
delete_repost <- function(
  tweet_id
) {

  # Get cached or refreshed token
  token <- authenticate_user()

  # Get authenticated user's ID
  user_req <- request("https://api.twitter.com/2/users/me") |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  
  # Get authenticated user's ID
  user_data <- resp_body_json(user_req)
  user_id <- user_data$data$id

  url <- paste0("https://api.twitter.com/2/users/", user_id, "/retweets/", tweet_id)

  # Perform repost
  response <- request(url) |>
    req_method("DELETE") |>
    req_auth_bearer_token(token$access_token) |>
    req_perform() |>
    resp_body_json()
}
