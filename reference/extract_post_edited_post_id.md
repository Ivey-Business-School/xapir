# Extract Post Edited Post ID from Timeline

Processes the timeline data retrieved from the X API to retrieve data on
previous versions of posts. The API lists a post's own id in its edit
history, so an unedited post lists only itself. Those self rows are
dropped: a row here means the post was edited, and `edited_post_id` is
an earlier version.

## Usage

``` r
extract_post_edited_post_id(timeline)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

## Value

A tibble containing structured edited post ID data.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post <- extract_post_edited_post_id(timeline)
} # }
```
