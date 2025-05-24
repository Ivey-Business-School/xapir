#' Get List
#'
#' @description
#' Get a User’s Owned Lists via the [owned list endpoint](https://docs.x.com/x-api/lists/get-a-users-owned-lists).
#'
#' @importFrom httr2 oauth_client oauth_flow_auth_code request req_auth_bearer_token req_perform resp_body_json
#' @param client_id A string containing the OAuth 2.0 client ID for authenticating
#'   with the X API. By default, this argument retrieves the token from the
#'   environment variable `X_CLIENT_ID` (via `Sys.getenv("X_CLIENT_ID")`).
#'   Adding your client ID to your `.Renviron` file ensures it is securely
#'   stored and accessible without needing to manually input it for each
#'   session.
#' @return A tibble containing the IDs of the lists and their names, or NULL if none found.
#' @examples
#' \dontrun{
#' lists <- get_list()
#' }
#' @export
get_list <- function(client_id = Sys.getenv("X_CLIENT_ID")) {

  # Create OAuth client 
  client <- oauth_client(
    id = client_id,
    token_url = "https://api.twitter.com/2/oauth2/token"
  )

  # Run authorization code flow with PKCE
  token <- oauth_flow_auth_code(
    client = client,
    auth_url = "https://twitter.com/i/oauth2/authorize",
    redirect_uri = "http://localhost:1410/",
    scope = "tweet.read users.read list.read offline.access",
    pkce = TRUE
  )

  # Get authenticated user's ID
  user_req <- request("https://api.twitter.com/2/users/me") |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  
  user_data <- resp_body_json(user_req)
  user_id <- user_data$data$id

  # Get lists owned by the user
  lists_req <- request(paste0("https://api.twitter.com/2/users/", user_id, "/owned_lists")) |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  
  lists_data <- resp_body_json(lists_req)

  if (!is.null(lists_data$data)) {
    df <- as.data.frame(do.call(rbind, lapply(lists_data$data, as.data.frame)))
    return(df)
  } else {
    message("No owned lists found.")
    return(NULL)
  }
}
