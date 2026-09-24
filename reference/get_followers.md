# Get Followers

Returns the users who follow an account via the [get followers
endpoint](https://docs.x.com/x-api/users/get-followers). Every user
returned is billed (US\$0.010 each in September 2026), so the function
says what the call can cost before it reads anything, and stops reading
at `max_users`. The default `max_users = 1000` is about US\$10.00; lower
it for a first look at a big account.

Give either `username` or `user_id`, not both. A `username` costs one
user read to turn the handle into an id before the followers are read.
When you already know the account's id, pass `user_id` and that read is
skipped.

## Usage

``` r
get_followers(
  username = NULL,
  user_id = NULL,
  max_results = 1000,
  max_users = 1000,
  pagination_token = NULL,
  sleep_time = 0,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  user_fields = default_user_fields()
)
```

## Arguments

- username:

  `character`; the name of the account on X without the "@" symbol.

- user_id:

  `character`; the account's X user id, as a string of digits. When
  given, the handle lookup is skipped and `username` must be `NULL`.

- max_results:

  `numeric`; the number of users per API call, between 1 and 1,000. The
  function stops before any request if the value is outside that range.

- max_users:

  `numeric`; the most users to read across all pages. Reading stops once
  this many have been returned. Default 1,000, about US\$10.00.

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
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "profile_banner_url", "public_metrics", "verified", "verified_type", "verified_followers_count", "subscription_type", "parody", "is_identity_verified", "url")`.
  Four more fields, `connection_status`, `confirmed_email`,
  `receives_your_dm` and `subscribes_to_you`, describe the account's
  relationship with the signed-in user; they need a user token and are
  not requested by default.

## Value

A tibble with one row per follower and the 24 columns described in
[`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
from `created_at` to `user_id`. An account with no followers gives the
same columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
followers <- get_followers("XDevelopers", max_users = 200)

# The same followers by id, with no user read for the handle
followers <- get_followers(user_id = "2244994945", max_users = 200)
} # }
```
