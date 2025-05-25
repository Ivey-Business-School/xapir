#' Delete a Post on X
#'
#' @description
#' Delete specified Post (in the path) by ID via the [delete a post 
#' endpoint](https://docs.x.com/x-api/posts/post-delete-by-post-id).
#'
#' @importFrom httr2 request req_url_path_append req_auth_bearer_token req_perform resp_body_json
#' @param post_id The ID of the post you want to delete.
#' @template client_id
#' @return A list containing the API response, typically `deleted = TRUE`.
#' @examples
#' \dontrun{
#' delete_from_x(post_id = targeted_post_id)
#' }
#' @export
delete_from_x <- function(
    post_id, 
    client_id = Sys.getenv("X_CLIENT_ID")
) {

  # Create OAuth 2.0 client
  client <- oauth_client(
    id = client_id,
    token_url = "https://api.twitter.com/2/oauth2/token"
  )

  # Run authorization code flow with PKCE
  token <- oauth_flow_auth_code(
    client = client,
    auth_url = "https://twitter.com/i/oauth2/authorize",
    redirect_uri = "http://localhost:1410/",
    scope = "tweet.read tweet.write users.read list.read offline.access",
    pkce = TRUE
  )

  # Construct request
  req <- request("https://api.twitter.com/2/tweets") |>
    req_url_path_append(post_id) |>
    req_auth_bearer_token(token$access_token) |>
    req_method("DELETE")

  # Perform request
  resp <- req_perform(req)
  result <- resp_body_json(resp)

  return(result)
}
