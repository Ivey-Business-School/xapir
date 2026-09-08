# Authenticate User

Authenticates with the X API using OAuth 2.0 Authorization Code Flow
with offline access. Falls back to re-authentication if cached token
fails.

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

A list containing the token and refresh token
