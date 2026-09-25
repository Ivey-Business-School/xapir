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

A tibble with one row and the 24 columns described in
[`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
from `created_at` to `user_id`.

## Examples

``` r
if (FALSE) { # \dontrun{
my_user <- get_my_user()
} # }
```
