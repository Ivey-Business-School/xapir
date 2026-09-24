# Get My User

Returns details about the signed-in user via the [get my user
endpoint](https://docs.x.com/x-api/users/get-my-user). Needs a user
token, so the first call opens a browser window to sign in.

## Usage

``` r
get_my_user(user_fields = default_user_fields())
```

## Arguments

- user_fields:

  `character`, `vector`; the fields to return for each user. Default:
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "profile_banner_url", "public_metrics", "verified", "verified_type", "verified_followers_count", "subscription_type", "parody", "is_identity_verified", "url")`.
  Four more fields, `connection_status`, `confirmed_email`,
  `receives_your_dm` and `subscribes_to_you`, describe the account's
  relationship with the signed-in user; they need a user token and are
  not requested by default.

## Value

A tibble with one row and the 24 columns described in
[`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
from `created_at` to `user_id`.

## Examples

``` r
if (FALSE) { # \dontrun{
my_user <- get_my_user()
} # }
```
