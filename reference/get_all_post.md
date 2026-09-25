# Get All Posts

Returns posts from the full archive, back to the first post in 2006,
that match a search query via the [full-archive search
endpoint](https://docs.x.com/x-api/posts/search-all-posts). The endpoint
needs pay-per-use or Enterprise access; on a tier without it the call
stops with the API's own message.

Every post returned is billed (US\$0.005 each in September 2026), so the
function says what the call can cost before it reads anything, and stops
reading at `max_posts`. Size a query with
[`get_all_post_count()`](https://Ivey-Business-School.github.io/xapir/reference/get_all_post_count.md)
first: a count is billed once, however many posts it covers.

## Usage

``` r
get_all_post(
  query,
  max_results = 100,
  max_posts = 500,
  start_time = NULL,
  end_time = NULL,
  since_id = NULL,
  until_id = NULL,
  sort_order = "relevancy",
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

  `numeric`; the number of posts per API call, between 10 and 500. The
  function stops before any request if the value is outside that range.

- max_posts:

  The most posts to read in this call. The API bills every post it
  returns (US\$0.005 each in September 2026), so the function prints the
  cap in posts and dollars before its first request and the total it
  read after the last page. Must be a finite number of 1 or more. The
  last page is trimmed so the result never holds more than this many
  posts. Your own data is billed at US\$0.001 an item when the signed-in
  account owns the app; the cost line says "(your own data)" when the
  package knows the account is yours, which it does after a sign-in or
  from `options(xapir.my_user_id = "<id>")`. When a price changes, set
  `options(xapir.prices = list(posts = <dollars>))` and the messages
  follow.

- start_time:

  The earliest date-time from which you want to get posts. Provide the
  value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
  [`iso_8601()`](https://Ivey-Business-School.github.io/xapir/reference/iso_8601.md)
  function will convert a string, date, or date-time object to the
  required format (e.g., `iso_8601("2024-10-10")`).

- end_time:

  The latest date-time from which you want to get posts.

- since_id:

  A post ID to limit the results to posts more recent than the specified
  ID.

- until_id:

  A post ID to limit the results to posts older than the specified ID.

- sort_order:

  The order of the posts returned: 'relevancy' or 'recency'.

- pagination_token:

  A string used to navigate backward through result pages. The X API
  provides this token when more results are available. Typically, you
  won\<80\>\<99\>t need to set this manually as the function handles it,
  but you can supply a pagination_token from a previous response to
  continue retrieving results beyond the last page, if desired.

- sleep_time:

  Seconds to pause between pages, `0` by default. A pause is optional:
  rate limits are handled for you. A 429 or a 5xx response is retried up
  to three times, waiting as long as the API asks. Any other error stops
  at once with the API's own message, so a mistyped handle or a bad
  token fails in seconds.

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
  `c("created_at", "text", "note_tweet", "article", "public_metrics", "geo", "attachments", "context_annotations", "entities", "lang", "possibly_sensitive", "edit_controls", "referenced_tweets", "reply_settings", "conversation_id", "in_reply_to_user_id", "author_id", "edit_history_tweet_ids", "community_id", "paid_partnership", "id")`.
  Fields are free; posts cost, so there is nothing to save by trimming
  this.

- user_fields:

  `character`, `vector`; the fields to return for each user. Default:
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "profile_banner_url", "public_metrics", "verified", "verified_type", "is_identity_verified", "url")`.
  Three fields the spec lists, `verified_followers_count`,
  `subscription_type` and `parody`, are refused to an app token ("not
  authorized to access 'parody' on the user", 24 September 2026), so
  they are not requested by default; their columns are NA. Ask for them
  with `user_fields = c(default_user_fields(), "parody")` when your
  token can read them. Four more, `connection_status`,
  `confirmed_email`, `receives_your_dm` and `subscribes_to_you`,
  describe the account's relationship with the signed-in user and need a
  user token.

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
  The API also offers `"attachments.media_source_tweet"`,
  `"article.cover_media"`, `"article.media_entities"` and
  `"edit_history_tweet_ids"`; no table reads them, so they are left out.
  Expansions are free.

## Value

A `list` of pages. Each page holds `data`, `includes` and `meta` as the
API returned them. Pass it to the `extract_*()` functions.

## Examples

``` r
if (FALSE) { # \dontrun{
pages <- get_all_post(
  "#SuperBowl lang:en",
  start_time = iso_8601("2020-02-01"),
  end_time = iso_8601("2020-02-03"),
  max_posts = 1000
)
posts <- extract_post(pages)
} # }
```
