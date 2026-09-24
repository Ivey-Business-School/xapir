#' @param max_results \code{numeric}; the number of posts per API call, between
#'   10 and 100. The function stops before any request if the value is outside
#'   that range. For a reader that returns one page this is also the most it
#'   can read, and the worst-case cost is printed before the request.
