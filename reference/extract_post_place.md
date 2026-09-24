# Extract Post Place and Geo Coordinates from Timeline

Processes the timeline data retrieved from the X API to wrangle the
place a post was tagged with, from `includes$places`, and its bounding
box. Each row is one post with a place tag. When the place's details are
not in `includes`, the row keeps its `post_id` and `place_id` and the
other columns are `NA`.

Each post appears once, even when it sits in one page's `data` and
another page's `includes$tweets`. The `data` copy wins.

## Usage

``` r
extract_post_place(timeline, include_referenced_posts = TRUE)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

- include_referenced_posts:

  Logical. Whether to include the posts in `includes$tweets` (the posts
  that were quoted, replied to or reposted). Defaults to TRUE.

## Value

A tibble with one row per tagged post and the columns `post_id`,
`place_id`, `full_name`, `country`, `country_code` and `place_type`
(character), and `west_longitude`, `south_latitude`, `east_longitude`
and `north_latitude` (numeric, the place's bounding box). A timeline
without place tags gives zero rows with the same columns.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_place <- extract_post_place(timeline)
} # }
```
