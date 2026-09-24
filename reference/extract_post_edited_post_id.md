# Extract Post Edited Post ID from Timeline

Processes the timeline data retrieved from the X API to retrieve data on
previous versions of posts. The API lists a post's own id in its edit
history, so an unedited post lists only itself. Those self rows are
dropped: a row here means the post was edited, and `edited_post_id` is
an earlier version.

Each post appears once, even when it sits in one page's `data` and
another page's `includes$tweets`. The `data` copy wins.

## Usage

``` r
extract_post_edited_post_id(timeline, include_referenced_posts = TRUE)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

- include_referenced_posts:

  Logical. Whether to include the posts in `includes$tweets` (the posts
  that were quoted, replied to or reposted). Defaults to TRUE.

## Value

A tibble with one row per earlier version per edited post and the
character columns `post_id` and `edited_post_id`. A timeline without
edited posts gives zero rows with the same columns.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_edited_post_id <- extract_post_edited_post_id(timeline)
} # }
```
