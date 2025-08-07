#' Delete Bookmark 
#'
#' @description
#' Removes a Post from the authenticated user’s Bookmarks by its ID
#' via the [delete bookmark endpoint](https://docs.x.com/x-api/bookmarks/delete-bookmark).
#' The User must match the User context authorizing the request
#'
#' @importFrom httr2 request req_auth_bearer_token req_perform resp_body_json req_body_json req_method
#' @template username
#' @param tweet_id ID of the tweet to be bookmarked.
#' @examples
#' \dontrun{
#' delete_bookmark(
#'  username = "Tesla", 
#'  tweet_id = "1234567890123456789"
#' )
#' }
#' @export
delete_bookmark <- function(
  username,
  tweet_id
) {
  # Get cached or refreshed token
  token <- authenticate_user()
  
  # Obtain the user_id of the user account
  url <- paste0("https://api.twitter.com/2/users/by/username/", username)
  req <- request(url) |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  resp <- resp_body_json(req)
  user_id <- resp$data$id
  
  # Bookmark the tweet
  url <- paste0("https://api.twitter.com/2/users/", user_id, "/bookmarks")
  req <- request(url) |>
    req_method("DELETE") |>
    req_auth_bearer_token(token$access_token) |>
    req_body_json(list(tweet_id = tweet_id)) |>
    req_perform()
  
  resp <- resp_body_json(req)
  return(resp)
}