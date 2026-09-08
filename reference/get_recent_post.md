# Get Recent Post

Returns Posts from the last 7 days that match a search query via the
[recent search endpoint](https://docs.x.com/x-api/posts/recent-search).

## Usage

``` r
get_recent_post(
  query,
  max_results = 100,
  max_posts = 3200,
  end_time = NULL,
  start_time = NULL,
  sort_order = "relevancy",
  until_id = NULL,
  since_id = NULL,
  pagination_token = NULL,
  sleep_time = 0,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  post_fields = default_post_fields(),
  user_fields = default_user_fields(),
  media_fields = default_media_fields(),
  poll_fields = default_poll_fields(),
  place_fields = default_place_fields(),
  expansions = default_expansions()
)
```

## Arguments

- query:

  The search to be made on X. You can find ways to build specific
  queries according to the [X API documentation
  website](https://docs.x.com/x-api/posts/search/integrate/build-a-query#types)

- max_results:

  `numeric`; the number of posts per API call, between 10 and 100. The
  function stops before any request if the value is outside that range.

- max_posts:

  The most posts to read in this call. The API bills every post it
  returns (US\$0.005 each in September 2026), so the function prints the
  cap in posts and dollars before its first request. The last page is
  trimmed so the result never holds more than this many posts.

- end_time:

  The latest date-time from which you want to get posts. Provide the
  value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
  [`iso_8601()`](https://Ivey-Business-School.github.io/xapir/reference/iso_8601.md)
  function will convert a string, date, or date-time object to the
  required format (e.g., `iso_8601("2024-10-10")`).

- start_time:

  The earliest date-time from which you want to get posts.

- sort_order:

  The order of the posts returned: 'relevancy' or 'recency'.

- until_id:

  A post ID to limit the results to posts older than the specified ID.

- since_id:

  A post ID to limit the results to posts more recent than the specified
  ID.

- pagination_token:

  A string used to navigate backward through result pages. The X API
  provides this token when more results are available. Typically, you
  won’t need to set this manually as the function handles it, but you
  can supply a pagination_token from a previous response to continue
  retrieving results beyond the last page, if desired.

- sleep_time:

  Seconds to pause between pages. Rate limits are handled for you: a 429
  or a 5xx response is retried up to three times, waiting as long as the
  API asks. Any other error stops at once with the API's own message, so
  a mistyped handle or a bad token fails in seconds.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- post_fields:

  `character`, `vector`; the fields to return for each post. The default
  asks for everything the twelve tables need:
  `c("created_at", "text", "note_tweet", "article", "public_metrics", "geo", "attachments", "context_annotations", "entities", "lang", "possibly_sensitive", "edit_controls", "referenced_tweets", "reply_settings", "conversation_id", "in_reply_to_user_id", "author_id", "edit_history_tweet_ids", "id")`.
  Fields are free; posts cost, so there is nothing to save by trimming
  this.

- user_fields:

  `character`, `vector`; the fields to return for each user. Default:
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "public_metrics", "verified", "verified_type", "is_identity_verified", "url")`.

- media_fields:

  `character`, `vector`; the fields to return for each media item.
  Default:
  `c("duration_ms", "height", "width", "preview_image_url", "type", "url", "alt_text", "public_metrics", "variants", "media_key")`.

- poll_fields:

  `character`, `vector`; the fields to return for each poll. Default:
  `c("end_datetime", "duration_minutes", "options", "voting_status", "id")`.

- place_fields:

  `character`, `vector`; the fields to return for each tagged place.
  Default:
  `c("country", "country_code", "full_name", "geo", "id", "place_type")`.

- expansions:

  `character`, `vector`; the related objects to return alongside each
  post. Default:
  `c("author_id", "entities.mentions.username", "referenced_tweets.id.author_id", "referenced_tweets.id", "in_reply_to_user_id", "attachments.media_keys", "attachments.poll_ids", "geo.place_id")`.
  Expansions are free.

## Value

A `list` of pages. Each page holds `data`, `includes` and `meta` as the
API returned them. Pass it to the `extract_*()` functions.

## Examples

``` r
if (FALSE) { # \dontrun{
tl <- get_recent_post("Developers")
} # }
```
