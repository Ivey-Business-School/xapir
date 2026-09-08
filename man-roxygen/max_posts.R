#' @param max_posts The most posts to read in this call. The API bills every
#'   post it returns (US$0.005 each in September 2026), so the function prints
#'   the cap in posts and dollars before its first request. The last page is
#'   trimmed so the result never holds more than this many posts.
