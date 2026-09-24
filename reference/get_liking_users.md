# Get Liking Users

Retrieves a list of Users who liked a specific Post by its ID via the
[get liking users
endpoint](https://docs.x.com/x-api/posts/get-liking-users). Needs a user
token, so the first call opens a browser window to sign in.

## Usage

``` r
get_liking_users(
  post_id,
  max_results = 100,
  max_users = 500,
  pagination_token = NULL,
  sleep_time = 0,
  user_fields = default_user_fields()
)
```

## Arguments

- post_id:

  The ID of the Post whose liking Users are to be retrieved.

- max_results:

  `numeric`; the number of posts per API call, between 10 and 100. The
  function stops before any request if the value is outside that range.
  For a reader that returns one page this is also the most it can read,
  and the worst-case cost is printed before the request.

- max_users:

  The most users to read in this call. The API bills every user it
  returns (US\$0.010 each in September 2026), so the function prints the
  cap in users and dollars before its first request and the total it
  read after the last page. Must be a finite number of 1 or more. Set
  `options(xapir.price_per_user = <dollars>)` when the price changes.

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

- user_fields:

  `character`, `vector`; the fields to return for each user. Default:
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "profile_banner_url", "public_metrics", "verified", "verified_type", "verified_followers_count", "subscription_type", "parody", "is_identity_verified", "url")`.
  Four more fields, `connection_status`, `confirmed_email`,
  `receives_your_dm` and `subscribes_to_you`, describe the account's
  relationship with the signed-in user; they need a user token and are
  not requested by default.

## Value

A `list` of pages. Each page holds `data` and `meta` as the API returned
them.

## Examples

``` r
if (FALSE) { # \dontrun{
users <- get_liking_users(post_id = "1234567890")
} # }
```
