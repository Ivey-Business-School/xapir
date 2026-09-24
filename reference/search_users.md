# Search Users

Finds accounts whose name, handle or bio match a search string via the
[search users endpoint](https://docs.x.com/x-api/users/search-users).
Needs a user token, so the first call opens a browser window to sign in.
Every user returned is billed, so the function says what the call can
cost before it reads anything, and stops reading at `max_users`.

## Usage

``` r
search_users(
  query,
  max_results = 100,
  max_users = 100,
  user_fields = default_user_fields()
)
```

## Arguments

- query:

  One search string, such as "electric vehicles".

- max_results:

  `numeric`; the number of users per API call, between 1 and 1000. The
  function stops before any request if the value is outside that range.

- max_users:

  `numeric`; the most users to read across all pages. Reading stops once
  this many have been returned. Default 100.

- user_fields:

  `character`, `vector`; the fields to return for each user. Default:
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "profile_banner_url", "public_metrics", "verified", "verified_type", "verified_followers_count", "subscription_type", "parody", "is_identity_verified", "url")`.
  Four more fields, `connection_status`, `confirmed_email`,
  `receives_your_dm` and `subscribes_to_you`, describe the account's
  relationship with the signed-in user; they need a user token and are
  not requested by default.

## Value

A tibble with one row per user and the 24 columns described in
[`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
from `created_at` to `user_id`. When nothing matches, the same columns
with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
ev_accounts <- search_users("electric vehicles", max_users = 50)
} # }
```
