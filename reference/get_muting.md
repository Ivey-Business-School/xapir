# Get Muting

Retrieves the users the signed-in user has muted via the [get muting
endpoint](https://docs.x.com/x-api/users/get-muting). Needs a user
token, so the first call opens a browser window to sign in. Every user
returned is billed, so the function says what the call can cost before
it reads anything, and stops reading at `max_users`.

## Usage

``` r
get_muting(
  user_fields = default_user_fields(),
  max_results = 100,
  max_users = 500,
  pagination_token = NULL
)
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

- max_results:

  `numeric`; the number of users per API call, between 10 and 100. The
  function stops before any request if the value is outside that range.

- max_users:

  `numeric`; the most users to read across all pages. Reading stops once
  this many have been returned. Default 500.

- pagination_token:

  A string used to navigate backward through result pages. The X API
  provides this token when more results are available. Typically, you
  won\<80\>\<99\>t need to set this manually as the function handles it,
  but you can supply a pagination_token from a previous response to
  continue retrieving results beyond the last page, if desired.

## Value

A tibble with one row per muted user and the 24 columns described in
[`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
from `created_at` to `user_id`. When nobody is muted, the same columns
with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
muted_users <- get_muting()
} # }
```
