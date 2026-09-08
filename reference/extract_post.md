# Extract Post Data from Timeline

Processes the timeline data retrieved from the X API to wrangle post
data, including metadata such as likes, reposts, replies, and
impressions.

Long posts (over 280 characters) arrive from the API with a truncated
`text` and the full text in `note_tweet`. This function puts the full
text in `text` and marks the row with `is_long_post = TRUE`.

A repost carries the original post's like, reply, quote, bookmark and
repost counts, so those five are set to `NA` on reposts. Its impression
count is its own and is kept.

Each post appears once, even when it sits in one page's `data` and
another page's `includes$tweets`. The `data` copy wins.

## Usage

``` r
extract_post(
  timeline,
  additional_cols = c("post_type", "post_url"),
  tz = "UTC",
  include_referenced_posts = TRUE
)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

- additional_cols:

  A character vector of derived columns to add. `"post_type"` classifies
  each post as Thread, Post, Quote post, Reply or Repost. `"post_url"`
  builds the address `https://x.com/<username>/status/<post_id>` from
  the author's handle in `includes$users`, falling back to
  `https://x.com/i/web/status/<post_id>` when the author is not there.

- tz:

  The time zone for `created_at`. The API returns UTC, and the default
  keeps it. Pass
  [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html) or a name
  such as `"America/Toronto"` to convert.

- include_referenced_posts:

  Logical. Whether to include the posts in `includes$tweets` (the posts
  that were quoted, replied to or reposted). Defaults to TRUE.

## Value

A tibble with one row per post, always with the same 22 columns.
`article_title` is NA unless the post is an X article.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post <- extract_post(timeline)
} # }
```
