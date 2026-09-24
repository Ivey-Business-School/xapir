# Get Usage

Returns how many posts your project has read against its monthly cap via
the [get usage endpoint](https://docs.x.com/x-api/usage/get-usage). This
is your own spend, so no cost line is printed: the pricing page does not
list the usage endpoints as billed.

## Usage

``` r
get_usage(days = 7, bearer_token = Sys.getenv("X_BEARER_TOKEN"))
```

## Arguments

- days:

  `numeric`; how many days of daily usage to return, between 1 and 90.
  Default 7.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

## Value

A tibble with one row: `project_id` (character), `project_cap` (integer,
posts a month), `project_usage` (integer, posts read so far this cycle)
and `cap_reset_day` (integer, the day of the month the count restarts).
The daily breakdown is attached as the attribute `"daily"`: a tibble
with `date` (Date) and `usage` (integer), one row per day, oldest first.
Read it with `attr(usage, "daily")`.

## Examples

``` r
if (FALSE) { # \dontrun{
usage <- get_usage()
attr(usage, "daily")
} # }
```
