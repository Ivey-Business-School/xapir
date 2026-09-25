# Get Spend

Turns the daily post reads that
[`get_usage()`](https://Ivey-Business-School.github.io/xapir/reference/get_usage.md)
reports (via the [get usage
endpoint](https://docs.x.com/x-api/usage/get-usage)) into a day-by-day
estimate of what they cost at the post price, and prints one summary
line. This is your own usage, so no cost line is printed: the pricing
page does not list the usage endpoints as billed.

The estimate counts post reads only, because the usage endpoint reports
nothing else: user reads, follower reads, counts and writes do not
appear here. It is also a ceiling. Reads of your own account's data bill
at the owned rate, and a post read twice on the same UTC day is billed
once, so the real bill is lower.
[`get_usage_credits()`](https://Ivey-Business-School.github.io/xapir/reference/get_usage_credits.md)
shows the actual balance.

## Usage

``` r
get_spend(days = 7, bearer_token = Sys.getenv("X_BEARER_TOKEN"))
```

## Arguments

- days:

  `numeric`; how many days back to report, between 1 and 90. Default 7.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

## Value

A tibble with one row per day, oldest first: `date` (Date), `posts`
(integer, posts read that day) and `dollars` (double, `posts` times the
post price). A period with no usage gives zero rows with the same
columns.

## Examples

``` r
if (FALSE) { # \dontrun{
spend <- get_spend()
#> Read 1,430 posts in the last 7 days, about $7.15 at the post price.

spend <- get_spend(days = 30)
sum(spend$dollars)
} # }
```
