# Extract Post Context Data from Timeline

Processes the timeline data retrieved from the X API to wrangle the
context annotations of each post: the domains (such as "Brand") and
entities (such as a company) the API tags the post with. Each row is one
annotation on one post.

Each post appears once, even when it sits in one page's `data` and
another page's `includes$tweets`. The `data` copy wins.

## Usage

``` r
extract_post_context(timeline, include_referenced_posts = TRUE)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

- include_referenced_posts:

  Logical. Whether to include the posts in `includes$tweets` (the posts
  that were quoted, replied to or reposted). Defaults to TRUE.

## Value

A tibble with one row per context annotation per post and the character
columns `post_id`, `domain_id`, `domain_name`, `domain_description`,
`entity_id`, `entity_name` and `entity_description`. A timeline without
context annotations gives zero rows with the same columns.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_context <- extract_post_context(timeline)
} # }
```
