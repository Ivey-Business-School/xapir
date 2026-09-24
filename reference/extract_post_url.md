# Extract Post URL Information from Timeline

Processes the timeline data retrieved from the X API to wrangle the
links in each post, such as the shortened `t.co` address, the address it
expands to, and the linked page's title and description. Each row is one
link in one post. The API lists the `pic.x.com` link of a post once per
attached photo; it is kept once here, since the media themselves are in
[`extract_post_media()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_media.md).

Each post appears once, even when it sits in one page's `data` and
another page's `includes$tweets`. The `data` copy wins.

## Usage

``` r
extract_post_url(timeline, include_referenced_posts = TRUE)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

- include_referenced_posts:

  Logical. Whether to include the posts in `includes$tweets` (the posts
  that were quoted, replied to or reposted). Defaults to TRUE.

## Value

A tibble with one row per link per post and the columns `post_id`,
`start`, `end`, `url`, `expanded_url`, `unwound_url`, `display_url`,
`title`, `description`, `status` and `image_url`. `start`, `end` and
`status` are integer; the rest are character. `image_url` is the first
preview image of the linked page, or `NA`. A timeline without links
gives zero rows with the same columns.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_url <- extract_post_url(timeline)
} # }
```
