# Get Recent Post Count

Returns Post Counts from the last 7 days that match a search query via
the [recent posts count
endpoint](https://docs.x.com/x-api/posts/recent-search-counts).

## Usage

``` r
get_recent_post_count(
  query,
  start_time = NULL,
  end_time = NULL,
  granularity = "hour",
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

  The earliest date-time from which you want to get posts.

- end_time:

  The latest date-time from which you want to get posts. Provide the
  value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
  [`iso_8601()`](https://Ivey-Business-School.github.io/xapir/reference/iso_8601.md)
  function will convert a string, date, or date-time object to the
  required format (e.g., `iso_8601("2024-10-10")`).

- granularity:

  The granularity for the search count results. This takes either the
  value 'minute', 'hour', or 'day'.

- is_local_tz:

  Logical. Convert `start` and `end` from UTC to the system time zone.

- drop_incomplete:

  Drops rows that do not contain a full granularity period amount of
  data.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

## Value

A tibble containing the number of posts.

## Examples

``` r
if (FALSE) { # \dontrun{
tl <- get_recent_post_count("Developers")
} # }
```
