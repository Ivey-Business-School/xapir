# Get List Posts

Returns the posts on a list's timeline via the [get list posts
endpoint](https://docs.x.com/x-api/lists/get-list-posts). Every post
returned is billed (US\$0.005 each in September 2026), so the function
says what the call can cost before it reads anything, and stops reading
at `max_posts`.

## Usage

``` r
get_list_posts(
  list_id,
  max_results = 100,
  max_posts = 500,
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

- list_id:

  The list's id, as a string of digits.

- max_results:

  `numeric`; the number of posts per API call, between 1 and 100. The
  function stops before any request if the value is outside that range.

- max_posts:

  The most posts to read in this call. The API bills every post it
  returns (US\$0.005 each in September 2026), so the function prints the
  cap in posts and dollars before its first request and the total it
  read after the last page. Must be a finite number of 1 or more. The
  last page is trimmed so the result never holds more than this many
  posts. When the price changes, set
  `options(xapir.price_per_post = <dollars>)` (and
  `xapir.price_per_user` for readers that return users) and the messages
  follow; the default is `0.005`.

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
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "profile_banner_url", "public_metrics", "verified", "verified_type", "verified_followers_count", "subscription_type", "parody", "is_identity_verified", "url")`.
  Four more fields, `connection_status`, `confirmed_email`,
  `receives_your_dm` and `subscribes_to_you`, describe the account's
  relationship with the signed-in user; they need a user token and are
  not requested by default.

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
pages <- get_list_posts(list_id = "1146654567674912769")
posts <- extract_post(pages)
} # }
```
