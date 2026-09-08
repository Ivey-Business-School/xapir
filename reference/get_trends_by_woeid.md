# Get Trends by WOEID

Retrieves trending topics for a specified location via its WOEID from
the [Get Trends by WOEID
endpoint](https://docs.x.com/x-api/trends/get-trends-by-woeid).

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

  Integer WOEID of the location to fetch trends for.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- max_trends:

  Integer for maximum results (1–50, default 20).

- trend_fields:

  Character vector of fields to include (e.g., "trend_name",
  "tweet_count").

## Value

A tibble with trend names and tweet count, or NULL if no data.

## Examples

``` r
if (FALSE) { # \dontrun{
tr <- get_trends_by_woeid(woeid = 4118)  # e.g., Toronto
} # }
```
