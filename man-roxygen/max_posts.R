#' @param max_posts The most posts to read in this call. The API bills every
#'   post it returns (US$0.005 each in September 2026), so the function prints
#'   the cap in posts and dollars before its first request and the total it
#'   read after the last page. Must be a finite number of 1 or more. The last
#'   page is trimmed so the result never holds more than this many posts. When
#'   the price changes, set `options(xapir.price_per_post = <dollars>)` (and
#'   `xapir.price_per_user` for readers that return users) and the messages
#'   follow; the default is `0.005`.
