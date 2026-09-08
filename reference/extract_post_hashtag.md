# Extract Post Hashtag Data from Timeline

Processes the timeline data retrieved from the X API to wrangle post
hashtag data.

## Usage

``` r
extract_post_hashtag(timeline)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

## Value

A tibble containing structured post context data.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post <- extract_post_hashtag(timeline)
} # }
```
