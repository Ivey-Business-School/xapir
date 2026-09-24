# Extract Media Information from Timeline

Processes the timeline data retrieved from the X API to wrangle media
information, such as images, videos, and GIFs attached to posts. Each
row is one media item attached to one post, so a post with three photos
gives three rows.

`alt_text` is the author's description of the media and is `NA` when
none was written. For a video or animated GIF, `url` and `bit_rate` come
from the `video/mp4` variant with the highest bit rate, which is the
best quality the API offers; for a photo, `url` is the image and
`bit_rate` is `NA`.

Each post appears once, even when it sits in one page's `data` and
another page's `includes$tweets`. The `data` copy wins.

## Usage

``` r
extract_post_media(timeline, include_referenced_posts = TRUE)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

- include_referenced_posts:

  Logical. Whether to include the posts in `includes$tweets` (the posts
  that were quoted, replied to or reposted). Defaults to TRUE.

## Value

A tibble with one row per media item per post and the columns `post_id`,
`media_id`, `type`, `view_count`, `duration_ms`, `height`, `width`,
`preview_image_url`, `url`, `alt_text` and `bit_rate`. Ids are
character; `view_count`, `duration_ms`, `height`, `width` and `bit_rate`
are integer. A timeline without media gives zero rows with the same
columns.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_media <- extract_post_media(timeline)
} # }
```
