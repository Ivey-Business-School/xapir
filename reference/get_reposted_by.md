# Get Reposted By

Returns the users who reposted a post via the [get reposted by
endpoint](https://docs.x.com/x-api/posts/get-reposted-by). Every user
returned is billed (US\$0.010 each in September 2026), so the function
says what the call can cost before it reads anything, and stops reading
at `max_users`.

## Usage

``` r
get_reposted_by(
  post_id,
  max_results = 100,
  max_users = 100,
  pagination_token = NULL,
  sleep_time = 0,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  user_fields = default_user_fields()
)
```

## Arguments

- post_id:

  The post's id, as a string of digits.

- max_results:

  `numeric`; the number of users per API call, between 1 and 100. The
  function stops before any request if the value is outside that range.

- max_users:

  `numeric`; the most users to read across all pages. Reading stops once
  this many have been returned. Default 100.

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

## Value

A tibble with one row per reposting user and the 24 columns described in
[`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
from `created_at` to `user_id`. A post nobody reposted gives the same
columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
reposters <- get_reposted_by("1354143047324299264")
} # }
```
