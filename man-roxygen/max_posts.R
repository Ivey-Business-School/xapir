#' @param max_posts The most posts to read in this call. The API bills every
#'   post it returns (US$0.005 each in September 2026), so the function prints
#'   the cap in posts and dollars before its first request and the total it
#'   read after the last page. Must be a finite number of 1 or more. The last
#'   page is trimmed so the result never holds more than this many posts. Your
#'   own data is billed at US$0.001 an item when the signed-in account owns
#'   the app; the cost line says "(your own data)" when the package knows the
#'   account is yours, which it does after a sign-in or from
#'   `options(xapir.my_user_id = "<id>")`. When a price changes, set
#'   `options(xapir.prices = list(posts = <dollars>))` and the messages
#'   follow.
