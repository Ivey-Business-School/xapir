#' Post to X
#'
#' @description
#' Causes the User to create a Post under the authorized account via the [create a post 
#' endpoint](https://docs.x.com/x-api/posts/creation-of-a-post).
#'
#' @importFrom httr2 oauth_client oauth_flow_auth_code request req_auth_bearer_token req_body_json req_perform resp_body_json
#' @param text The tweet text (max 280 characters)
#' @param for_super_followers_only Exclusive Tweet for super followers.
#' @param geo Place ID being attached to the Tweet for geo location.
#' @param media Media information being attached to created Tweet. This is mutually exclusive from 
#' Quote Tweet Id, Poll, and Card URI.
#' @param nullcast Nullcasted (promoted-only) Posts do not appear in the public timeline and are not served to followers.
#' @param poll Poll options for a Tweet with a poll. This is mutually exclusive from Media, Quote Tweet Id, and Card URI.
#' @param reply Tweet information of the Tweet being replied to.
#' @param reply_settings Settings to indicate who can reply to the Tweet.
#' @template client_id 
#' @examples
#' \dontrun{
#' post_to_x(text = "Hello, world!")
#' }
#' @export
post_to_x <- function(
    text,
    for_super_followers_only = FALSE,
    geo = NULL,
    media = NULL, 
    nullcast = FALSE,
    poll = NULL,
    reply = NULL,
    reply_settings = NULL,
    client_id = Sys.getenv("X_CLIENT_ID")
) {

  if (nchar(text) > 280) {
    stop("Tweet exceeds 280 characters.")
  }

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

  # Build JSON payload
  body <- list(text = text)

  if (!is.null(for_super_followers_only) && for_super_followers_only)
    body$for_super_followers_only <- TRUE

  if (!is.null(geo)) body$geo <- geo
  if (!is.null(media)) body$media <- media
  if (!is.null(nullcast) && nullcast) body$nullcast <- TRUE
  if (!is.null(poll)) body$poll <- poll
  if (!is.null(reply)) body$reply <- reply
  if (!is.null(reply_settings)) body$reply_settings <- reply_settings

  # Make API request
  response <- request("https://api.twitter.com/2/tweets") |>
    req_auth_bearer_token(token$access_token) |>
    req_body_json(body) |>
    req_perform() |>
    resp_body_json()
}
