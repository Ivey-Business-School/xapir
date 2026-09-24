# Extract Post Entity Annotation Data from Timeline

Processes the timeline data retrieved from the X API to wrangle the
entity annotations of each post: the people, places, products and
organizations the API recognizes in the text, with the API's confidence
and where the words sit in the text. Each row is one annotation on one
post.

Each post appears once, even when it sits in one page's `data` and
another page's `includes$tweets`. The `data` copy wins.

## Usage

``` r
extract_post_entity_annotation(timeline, include_referenced_posts = TRUE)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

- include_referenced_posts:

  Logical. Whether to include the posts in `includes$tweets` (the posts
  that were quoted, replied to or reposted). Defaults to TRUE.

## Value

A tibble with one row per entity annotation per post and the columns
`post_id`, `normalized_text` and `type` (character), `probability`
(numeric, 0 to 1), and `start` and `end` (integer positions in the post
text). A timeline without entity annotations gives zero rows with the
same columns.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_entity_annotation <- extract_post_entity_annotation(timeline)
} # }
```
