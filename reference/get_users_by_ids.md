# Get Users by IDs

Retrieves details of up to 100 users by their ids via the [get users by
IDs endpoint](https://docs.x.com/x-api/users/get-users-by-ids). Every
user returned is billed, so the function says what the call can cost
before it reads anything.

An id the API cannot find does not stop the call: the users it did find
are returned, and one warning names each id that was not.

## Usage

``` r
get_users_by_ids(
  user_ids,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  user_fields = default_user_fields(),
  expansions = NULL
)
```

## Arguments

- user_ids:

  A character vector of up to 100 user ids, each a string of digits.
  Keep ids as text: as numbers they lose digits.

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

- expansions:

  Not used by this endpoint. Accepted so that older code keeps running.

## Value

A tibble with one row per user and the 24 columns described in
[`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
from `created_at` to `user_id`. When no id is found, the same columns
with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
users <- get_users_by_ids(c("783214", "2244994945"))
} # }
```
