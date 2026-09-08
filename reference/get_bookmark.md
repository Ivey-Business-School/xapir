# Get Bookmark

Retrieves a list of Posts bookmarked by the authenticated user via the
[get bookmark
endpoint](https://docs.x.com/x-api/bookmarks/get-bookmarks). Needs a
user token, so the first call opens a browser window to sign in.

## Usage

``` r
get_bookmark(
  username,
  max_results = 100,
  max_posts = 500,
  pagination_token = NULL,
  sleep_time = 0,
  post_fields = default_post_fields(),
  user_fields = default_user_fields(),
  media_fields = default_media_fields(),
  poll_fields = default_poll_fields(),
  place_fields = default_place_fields(),
  expansions = default_expansions()
)
```

## Arguments

- username:

  `character`; the name of the account on X without the "@" symbol.

- max_results:

  `numeric`; the number of posts per API call, between 10 and 100. The
  function stops before any request if the value is outside that range.

- max_posts:

  The most posts to read in this call. The API bills every post it
  returns (US\$0.005 each in September 2026), so the function prints the
  cap in posts and dollars before its first request. The last page is
  trimmed so the result never holds more than this many posts.

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
bookmarks <- get_bookmark("XDevelopers")
} # }
```
