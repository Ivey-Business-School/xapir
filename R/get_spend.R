#' Get Spend
#'
#' @description
#' Turns the daily post reads that `get_usage()` reports (via the
#' [get usage endpoint](https://docs.x.com/x-api/usage/get-usage)) into a
#' day-by-day estimate of what they cost at the post price, and prints one
#' summary line. This is your own usage, so no cost line is printed: the
#' pricing page does not list the usage endpoints as billed.
#'
#' The estimate counts post reads only, because the usage endpoint reports
#' nothing else: user reads, follower reads, counts and writes do not
#' appear here. It is also a ceiling. Reads of your own account's data bill
#' at the owned rate, and a post read twice on the same UTC day is billed
#' once, so the real bill is lower. `get_usage_credits()` shows the actual
#' balance.
#'
#' @importFrom tibble tibble
#' @param days \code{numeric}; how many days back to report, between 1 and
#'   90. Default 7.
#' @template bearer_token
#' @return A tibble with one row per day, oldest first: `date` (Date),
#'   `posts` (integer, posts read that day) and `dollars` (double, `posts`
#'   times the post price). A period with no usage gives zero rows with the
#'   same columns.
#' @examples
#' \dontrun{
#' spend <- get_spend()
#' #> Read 1,430 posts in the last 7 days, about $7.15 at the post price.
#'
#' spend <- get_spend(days = 30)
#' sum(spend$dollars)
#' }
#' @export
get_spend <- function(
  days         = 7,
  bearer_token = Sys.getenv("X_BEARER_TOKEN")
) {
  check_token(bearer_token)
  check_usage_days(days)

  daily <- attr(get_usage(days = days, bearer_token = bearer_token), "daily")
  price <- x_price("posts")

  spend <- tibble(
    date    = as.Date(daily$date),
    posts   = as.integer(daily$usage),
    dollars = as.double(daily$usage) * price
  )
  spend <- spend[order(spend$date), ]

  total <- sum(spend$posts, na.rm = TRUE)
  message(sprintf(
    "Read %s posts in the last %s days, about $%s at the post price.",
    format(total, big.mark = ",", scientific = FALSE),
    format(as.integer(days)),
    dollars(total * price)
  ))
  spend
}
