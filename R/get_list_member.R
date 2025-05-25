#' Get List Members
#'
#' @description
#' Returns a list of Users that are members of a List by the provided List ID via the [get list 
#' members](https://docs.x.com/x-api/users/returns-user-objects-that-are-members-of-a-list-by-the-provided-list-id).
#'
#' @importFrom httr2 oauth_client oauth_flow_auth_code request req_auth_bearer_token req_perform resp_body_json
#' @param list_id A string representing the unique ID of the list.
#' @template client_id 
#' @return A tibble with user details (ID, name, username) of list members, or NULL if none found.
#' @examples
#' \dontrun{
#' list_members <- get_list_members(list_ID)
#' }
#' @export
get_list_member <- function(
    list_id, 
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
    scope = "tweet.read users.read list.read offline.access",
    pkce = TRUE
  )

  # Make request to list members endpoint
  url <- paste0("https://api.twitter.com/2/lists/", list_id, "/members")

  members_req <- request(url) |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()

  members_data <- resp_body_json(members_req)

  # Extract member data
  if (!is.null(members_data$data)) {
    df <- as.data.frame(do.call(rbind, lapply(members_data$data, as.data.frame)))
    return(df)
  } else {
    message("No members found in this list.")
    return(NULL)
  }
}