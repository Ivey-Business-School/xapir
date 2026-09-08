# Retrieve Quote Posts for a Given Post

Returns a variety of information about each Post that quotes the Post
specified by the requested ID via the [quote tweets
endpoint](https://docs.x.com/x-api/posts/retrieve-posts-that-quote-a-post).

## Usage

``` r
get_quote_post(
  post_id,
  max_results = 100,
  exclude = NULL,
  pagination_token = NULL,
  post_fields = default_post_fields(),
  user_fields = default_user_fields(),
  media_fields = default_media_fields(),
  poll_fields = default_poll_fields(),
  place_fields = default_place_fields(),
  expansions = default_expansions(),
  bearer_token = Sys.getenv("X_BEARER_TOKEN")
)
```

## Arguments

- post_id:

  The ID of the post whose quote posts you want.

- max_results:

  `numeric`; the number of posts per API call, between 10 and 100. The
  function stops before any request if the value is outside that range.

- exclude:

  A comma-separated list of the types of posts to exclude from the
  response (e.g., "retweets", "replies", or "retweets,replies").

- pagination_token:

  A string used to navigate backward through result pages. The X API
  provides this token when more results are available. Typically, you
  won’t need to set this manually as the function handles it, but you
  can supply a pagination_token from a previous response to continue
  retrieving results beyond the last page, if desired.

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

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

## Value

A `list` holding one page, in the same shape as
[`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md)
returns, so the `extract_*()` functions accept it.

## Examples

``` r
if (FALSE) { # \dontrun{
get_quote_post(post_id = "20", max_results = 100)
} # }
```
