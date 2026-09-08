#' @param sleep_time Seconds to pause between pages. Rate limits are handled
#'   for you: a 429 or a 5xx response is retried up to three times, waiting as
#'   long as the API asks. Any other error stops at once with the API's own
#'   message, so a mistyped handle or a bad token fails in seconds.
