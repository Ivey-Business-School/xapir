# Extract Post Poll Option Information from Timeline

Processes the timeline data retrieved from the X API to wrangle poll
option information, such as poll IDs, options, and voting details. Posts
are read from `data` and from `includes$tweets`, so a poll on a quoted
post keeps its `post_id`.

## Usage

``` r
extract_post_poll_option(timeline)
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
polls <- extract_post_poll_option(timeline)
} # }
```
