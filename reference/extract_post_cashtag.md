# Extract Post Cashtag Data from Timeline

Processes the timeline data retrieved from the X API to wrangle post
cashtag data, including metadata relating to the ticker symbol and its
position in the post.

## Usage

``` r
extract_post_cashtag(timeline)
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
post <- extract_post_cashtag(timeline)
} # }
```
