#' Authenticate User
#'
#' @description
#' Authenticates with the X API using OAuth 2.0 Authorization Code Flow with offline access.
#' Falls back to re-authentication if cached token fails.
#'
#' @importFrom httr2 oauth_client oauth_flow_auth_code
#' @template client_id 
#' @return A list containing the token
#' @keywords internal
authenticate_user <- function(
  client_id = Sys.getenv("X_CLIENT_ID")
) {
  # Check in-memory token
  if (!is.null(.x_env$token)) {
    token <- .x_env$token
    if (!is.null(token$expires_at) && Sys.time() < token$expires_at) {
      return(token)
    } else {
      message("In-memory token expired. Re-authenticating...")
    }
  }

  # Re-authenticate
  client <- httr2::oauth_client(
    id = client_id,
    token_url = "https://api.twitter.com/2/oauth2/token"
  )

  token <- httr2::oauth_flow_auth_code(
    client = client,
    auth_url = "https://twitter.com/i/oauth2/authorize",
    redirect_uri = "http://localhost:1410",
    scope = "tweet.read tweet.write users.read list.read follows.write"
  )

  .x_env$token <- token
  return(token)
}
