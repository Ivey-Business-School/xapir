# Extract Post Poll Option Information from Timeline

Processes the timeline data retrieved from the X API to wrangle poll
option information, such as poll IDs, options, and voting details. Each
row is one option of one poll on one post, so a two-option poll gives
two rows. A poll whose post is not in the response has nothing to attach
to and is left out.

Each post appears once, even when it sits in one page's `data` and
another page's `includes$tweets`. The `data` copy wins.

## Usage

``` r
extract_post_poll_option(timeline, include_referenced_posts = TRUE)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

- include_referenced_posts:

  Logical. Whether to include the posts in `includes$tweets` (the posts
  that were quoted, replied to or reposted). Defaults to TRUE.

## Value

A tibble with one row per poll option per post and the columns `post_id`
and `poll_id` (character), `position` (integer, the option's order in
the poll), `label` (character), `votes` (integer), `duration_minutes`
(integer), `end_datetime` (POSIXct, UTC) and `voting_status` (character,
"open" or "closed"). A timeline without polls gives zero rows with the
same columns.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_poll_option <- extract_post_poll_option(timeline)
} # }
```
