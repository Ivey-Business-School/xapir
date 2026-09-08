# Extract Post Entity Annotation Data from Timeline

Processes the timeline data retrieved from the X API to wrangle post
entity annotation data, including metadata relating to the type and its
probability.

## Usage

``` r
extract_post_entity_annotation(timeline)
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
post <- extract_post_entity_annotation(timeline)
} # }
```
