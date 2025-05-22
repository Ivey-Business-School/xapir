#' Get Owned Lists from X (Twitter) via OAuth 2.0
#'
#' @description
#' Authenticates via OAuth 2.0 Authorization Code with PKCE and retrieves all lists owned by the authenticated user.
#'
#' @param client_id Your app's Client ID (from developer portal).
#' @return A data frame of lists you own, or NULL if none found.
#' @importFrom httr2 oauth_client oauth_flow_auth_code request req_auth_bearer_token req_perform resp_body_json
#' @export
get_list <- function(client_id = Sys.getenv("X_CLIENT_ID")) {
  library(httr2)

  # Step 1: Create OAuth client (no secret for PKCE)
  client <- oauth_client(
    id = client_id,
    token_url = "https://api.twitter.com/2/oauth2/token"
  )

  # Step 2: Run authorization code flow with PKCE
  token <- oauth_flow_auth_code(
    client = client,
    auth_url = "https://twitter.com/i/oauth2/authorize",
    redirect_uri = "http://localhost:1410/",
    scope = "tweet.read users.read list.read offline.access",
    pkce = TRUE
  )

  # Step 3: Get authenticated user's ID
  user_req <- request("https://api.twitter.com/2/users/me") |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  
  user_data <- resp_body_json(user_req)
  user_id <- user_data$data$id

  # Step 4: Get lists owned by the user
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
