# Authenticate User

Signs in to the X API with OAuth 2.0 (authorization code flow with PKCE)
and returns the token every user-context function uses. The first call
opens a browser window to approve the app. The token is then cached on
disk, so later calls, and later R sessions, reuse it without a browser.
When the access token expires it is refreshed in the background; if the
refresh fails, the browser flow runs again.

The token is stored under
[`httr2::oauth_cache_path()`](https://httr2.r-lib.org/reference/oauth_cache_path.html)
in a folder named `xapir`. To sign in as a different account, or after
changing the scopes the package asks for, delete that folder and call a
user function again:
`unlink(file.path(httr2::oauth_cache_path(), "xapir"), recursive = TRUE)`.

## Usage

``` r
authenticate_user(client_id = Sys.getenv("X_CLIENT_ID"))
```

## Arguments

- client_id:

  A string containing the OAuth 2.0 client ID for authenticating with
  the X API. By default, this argument retrieves the token from the
  environment variable `X_CLIENT_ID` (via `Sys.getenv("X_CLIENT_ID")`).
  Adding your client ID to your `.Renviron` file ensures it is securely
  stored and accessible without needing to manually input it for each
  session.

## Value

An httr2 OAuth token (class `httr2_token`) with `$access_token`, and
`$refresh_token` when the app grants offline access.

## Examples

``` r
if (FALSE) { # \dontrun{
token <- authenticate_user()
token$access_token
} # }
```
