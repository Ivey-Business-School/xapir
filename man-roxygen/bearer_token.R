#' @param bearer_token A string containing the bearer token for authenticating
#'   with the X API. By default, this argument retrieves the token from the
#'   environment variable `X_BEARER_TOKEN` (via `Sys.getenv("X_BEARER_TOKEN")`).
#'   Adding your bearer token to your `.Renviron` file keeps it out of your
#'   scripts and available in every session.
