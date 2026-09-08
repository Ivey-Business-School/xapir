# Extract Post Place and Geo Coordinates from Timeline

Processes the timeline data retrieved from the X API to wrangle place
information and exact geo-coordinates attached to posts.

## Usage

``` r
extract_post_place(timeline)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

## Value

A tibble containing structured place and coordinate data.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_places <- extract_post_place(timeline)
} # }
```
