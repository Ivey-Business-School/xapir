# Get Post by IDs

Returns a variety of information about the Post specified by the
requested ID via the [get posts by IDs
endpoint](https://docs.x.com/x-api/posts/get-posts-by-ids).

## Usage

``` r
get_post(
  post_ids,
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

- post_ids:

  The IDs of the posts to retrieve, as a character vector of up to 100
  ids. Keep them as text: as numbers they lose digits. The function
  stops before any request when an id is not a string of digits or there
  are more than 100.

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

A `list` holding one page, in the same shape as
[`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md)
returns, so the `extract_*()` functions accept it. At most one post per
id is returned, and that worst case is printed in posts and dollars
before the request.

## Examples

``` r
if (FALSE) { # \dontrun{
post <- get_post(c("1234567890123456789"))
} # }
```
