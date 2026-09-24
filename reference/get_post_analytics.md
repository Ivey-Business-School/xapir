# Get Post Analytics

Retrieves engagement metrics over time for up to 100 of the signed-in
account's own posts via the [get post analytics
endpoint](https://docs.x.com/x-api/posts/get-post-analytics). Needs a
user token, so the first call opens a browser window to sign in.

The pricing page does not list this endpoint, so the function prints no
cost line. Check your usage on the developer console after the first
call.

## Usage

``` r
get_post_analytics(post_ids, start_time, end_time, granularity = "total")
```

## Arguments

- post_ids:

  A character vector of up to 100 post ids. They must be posts by the
  account that signed in.

- start_time:

  The start of the period, as an ISO 8601 string such as
  `"2026-09-01T00:00:00Z"` or a date-time object.

- end_time:

  The end of the period, in the same form.

- granularity:

  "total" (the default) for one row per post, or "hourly", "daily" or
  "weekly" for one row per post per period.

## Value

A tibble with one row per post per period: `post_id`, `timestamp`
(POSIXct, UTC; `NA` for a total) and one integer column per metric:
`impressions`, `engagements`, `likes`, `reposts`, `replies`, `quotes`,
`bookmarks`, `follows`, `unfollows`, `url_clicks`,
`user_profile_clicks`, `media_views`, `detail_expands`,
`permalink_clicks`, `hashtag_clicks`, `shares`, `app_opens`,
`app_install_attempts`, `email_post` and `unlikes`. The API calls
reposts `retweets`, quotes `quote_tweets` and email shares
`email_tweet`; the table uses the post names. When the API returns
nothing, the same columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
daily <- get_post_analytics(
  post_ids    = c("1234567890123456789", "1234567890123456790"),
  start_time  = "2026-09-01T00:00:00Z",
  end_time    = "2026-09-08T00:00:00Z",
  granularity = "daily"
)
} # }
```
