#' Hide Reply on X
#'
#' @description
#' Hides or unhides a reply to a conversation owned by the authenticated user
#'  via the [hide reply endpoint](https://docs.x.com/x-api/posts/hide-reply).
#'
#' @importFrom httr2 request req_auth_bearer_token req_body_json req_method req_perform resp_body_json
#' @param reply_id The ID of the reply to be hidden. Must be a reply to a post authored by the authenticating user.
#' @param hidden Indicates whether the reply should be hidden (TRUE) or unhidden (FALSE). Defaults to TRUE.
#' @examples
#' \dontrun{
#' # Hide a reply
#' hide_reply(reply_id = "1234567890123456789")
#' 
#' # Unhide a reply
#' hide_reply(reply_id = "1234567890123456789", hidden = FALSE)
#' }
#' @export
hide_reply <- function(
  reply_id, 
  hidden = TRUE
) {
  # Get cached or refreshed token
  token <- authenticate_user()
  
  # Construct the endpoint URL
  url <- paste0("https://api.twitter.com/2/tweets/", reply_id, "/hidden")
  
  # Perform hide/unhide request
  response <- request(url) |>
    req_method("PUT") |>
    req_auth_bearer_token(token$access_token) |>
    req_body_json(list(hidden = hidden)) |>
    req_perform() |>
    resp_body_json()
  
}