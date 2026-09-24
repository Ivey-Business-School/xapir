# Extract Post Mention Data from Timeline

Processes the timeline data retrieved from the X API to wrangle the
users mentioned in each post and where the mention sits in the text.
Each row is one mention in one post.

Each post appears once, even when it sits in one page's `data` and
another page's `includes$tweets`. The `data` copy wins.

## Usage

``` r
extract_post_mention(timeline, include_referenced_posts = TRUE)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

- include_referenced_posts:

  Logical. Whether to include the posts in `includes$tweets` (the posts
  that were quoted, replied to or reposted). Defaults to TRUE.

## Value

A tibble with one row per mention per post and the columns `post_id`,
`username` and `user_id` (character), and `start` and `end` (integer
positions in the post text). A timeline without mentions gives zero rows
with the same columns.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_mention <- extract_post_mention(timeline)
} # }
```
