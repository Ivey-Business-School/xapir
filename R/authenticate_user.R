#' Authenticate User
#'
#' @description
#' Signs in to the X API with OAuth 2.0 (authorization code flow with PKCE)
#' and returns the token every user-context function uses. The first call
#' opens a browser window to approve the app. The token is then cached on
#' disk, so later calls, and later R sessions, reuse it without a browser.
#' When the access token expires it is refreshed in the background; if the
#' refresh fails, the browser flow runs again.
#'
#' The token is stored under `httr2::oauth_cache_path()` in a folder named
#' `xapir`. To sign in as a different account, or after changing the scopes
#' the package asks for, delete that folder and call a user function again:
#' `unlink(file.path(httr2::oauth_cache_path(), "xapir"), recursive = TRUE)`.
#'
#' @template client_id
#' @return An httr2 OAuth token (class `httr2_token`) with `$access_token`,
#'   and `$refresh_token` when the app grants offline access.
#' @examples
#' \dontrun{
#' token <- authenticate_user()
#' token$access_token
#' }
#' @keywords internal
authenticate_user <- function(
  client_id = Sys.getenv("X_CLIENT_ID")
) {

  if (is.null(client_id) || length(client_id) != 1 || is.na(client_id) ||
      !nzchar(client_id)) {
    stop(
      "No client id. Put X_CLIENT_ID=<your OAuth 2.0 client id> in your ",
      ".Renviron file, restart R, and try again. The client id is on the ",
      "app's page in the X developer portal.",
      call. = FALSE
    )
  }

  # A fixed name keeps the disk cache in the same place from session to
  # session; the default would hash the client id instead.
  client <- oauth_client(
    id        = client_id,
    token_url = "https://api.x.com/2/oauth2/token",
    name      = "xapir"
  )

  oauth_token_cached(
    client      = client,
    flow        = oauth_flow_auth_code,
    flow_params = list(
      auth_url     = "https://x.com/i/oauth2/authorize",
      redirect_uri = "http://localhost:1410",
      scope        = x_oauth_scope(),
      pkce         = TRUE
    ),
    cache_disk  = TRUE
  )
}
