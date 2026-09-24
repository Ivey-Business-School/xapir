# Get Trends by WOEID

Retrieves the trending topics for a location, given its WOEID (Yahoo's
"Where On Earth" id), via the [get trends by WOEID
endpoint](https://docs.x.com/x-api/trends/get-trends-by-woeid). For
example, 1 is worldwide, 23424977 is the United States and 4118 is
Toronto.

## Usage

``` r
get_trends_by_woeid(
  woeid,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  max_trends = 20,
  trend_fields = c("trend_name", "tweet_count")
)
```

## Arguments

- woeid:

  The location's WOEID, one number or a string of digits.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- max_trends:

  The most trends to return, between 1 and 50. Default 20.

- trend_fields:

  `character`, `vector`; the fields to return for each trend. The API
  calls the post count `tweet_count`.

## Value

A tibble with one row per trend: `trend_name` and `post_count` (the
number of posts on the topic). The API often sends no count at all, and
then `post_count` is `NA` for every row; the names are still the trends.
A location with no trends gives the same columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
trends <- get_trends_by_woeid(woeid = 4118)  # Toronto
} # }
```
