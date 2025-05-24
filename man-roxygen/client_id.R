#' @param client_id A string containing the OAuth 2.0 client ID for authenticating
#'   with the X API. By default, this argument retrieves the token from the
#'   environment variable `X_CLIENT_ID` (via `Sys.getenv("X_CLIENT_ID")`).
#'   Adding your client ID to your `.Renviron` file ensures it is securely
#'   stored and accessible without needing to manually input it for each
#'   session.
