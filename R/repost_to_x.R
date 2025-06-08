#' Repost a Post on X
#'
#' @description
#' Causes the User (in the path) to repost the specified Post. The User in the path must match the User 
#' context authorizing the request. This is done via the [retweet endpoint](https://docs.x.com/x-api/posts/causes-the-user-in-the-path-to-repost-the-specified-post).
#' 
#' @importFrom httr2 oauth_client oauth_flow_auth_code request req_auth_bearer_token req_body_json req_perform resp_body_json
#' @param tweet_id The ID of the post to be reposted.
#' @return A list containing the API response.
#' @examples
#' \dontrun{
#' repost_to_x(tweet_id = "20")
#' }
#' @export
repost_to_x <- function(
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

  url <- paste0("https://api.twitter.com/2/users/", user_id, "/retweets")

  # Perform repost
  response <- request(url) |>
    req_auth_bearer_token(token$access_token) |>
    req_body_json(list(tweet_id = tweet_id)) |>
    req_perform() |>
    resp_body_json()

  return(response)
}
