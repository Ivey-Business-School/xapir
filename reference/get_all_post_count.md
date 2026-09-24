# Get All Post Count

Returns how many posts in the full archive, back to 2006, match a search
query in each period via the [full-archive post counts
endpoint](https://docs.x.com/x-api/posts/get-count-of-all-posts). The
endpoint needs pay-per-use or Enterprise access; on a tier without it
the call stops with the API's own message.

A count request is billed once (US\$0.010 in September 2026), however
many posts it counts, and returns no posts at all. A long range comes
back in pages; the function follows them until the API has no more and
prints how many requests it made. Use it to size a query before paying
for
[`get_all_post()`](https://Ivey-Business-School.github.io/xapir/reference/get_all_post.md).

## Usage

``` r
get_all_post_count(
  query,
  start_time = NULL,
  end_time = NULL,
  granularity = "day",
  is_local_tz = TRUE,
  drop_incomplete = TRUE,
  bearer_token = Sys.getenv("X_BEARER_TOKEN")
)
```

## Arguments

- query:

  The search to be made on X. You can find ways to build specific
  queries according to the [X API documentation
  website](https://docs.x.com/x-api/posts/search/integrate/build-a-query#types)

- start_time:

  The earliest date-time from which you want to count posts. Provide the
  value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
  [`iso_8601()`](https://Ivey-Business-School.github.io/xapir/reference/iso_8601.md)
  function will convert a string, date, or date-time object to the
  required format (e.g., `iso_8601("2024-10-10")`). Without it the count
  starts 30 days ago.

- end_time:

  The latest date-time to which you want to count posts.

- granularity:

  The period each count covers: `"minute"`, `"hour"` or `"day"`. The
  function stops before any request on anything else.

- is_local_tz:

  Logical. Convert `start` and `end` from UTC to the system time zone.

- drop_incomplete:

  Drops the first and last rows, which cover only part of a
  `granularity` period.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

## Value

A tibble with one row per period: `start` and `end` (date-times) and
`post_count` (integer). When the query matched nothing, the tibble has
the same three columns and no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
counts <- get_all_post_count(
  "#SuperBowl lang:en",
  start_time = iso_8601("2020-01-01"),
  end_time = iso_8601("2020-03-01")
)
} # }
```
