# Extract Post URL Information from Timeline

Processes the timeline data retrieved from the X API to wrangle poll URL
information, such as the URL, title, and description.

## Usage

``` r
extract_post_url(timeline)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

## Value

A tibble containing structured poll data.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
polls <- extract_post_url(timeline)
} # }
```
