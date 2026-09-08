# Get Reposts of Me

Retrieves a list of Posts that repost content from the authenticated
user via the [get reposts of me
endpoint](https://docs.x.com/x-api/users/get-reposts-of-me). Needs a
user token, so the first call opens a browser window to sign in.

## Usage

``` r
get_repost_of_me(
  max_results = 100,
  post_fields = default_post_fields(),
  user_fields = default_user_fields(),
  media_fields = default_media_fields(),
  poll_fields = default_poll_fields(),
  place_fields = default_place_fields(),
  expansions = default_expansions()
)
```

## Arguments

- max_results:

  `numeric`; the number of posts per API call, between 10 and 100. The
  function stops before any request if the value is outside that range.

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

A `list` holding one page, in the same shape as
[`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md)
returns, so the `extract_*()` functions accept it.

## Examples

``` r
if (FALSE) { # \dontrun{
post <- get_repost_of_me()
} # }
```
