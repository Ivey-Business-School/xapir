# Get Users by Usernames

Retrieves details of up to 100 users by their usernames via the [get
users by usernames
endpoint](https://docs.x.com/x-api/users/get-users-by-usernames). Every
user returned is billed, so the function says what the call can cost
before it reads anything.

A handle the API cannot find does not stop the call: the users it did
find are returned, and one warning names each handle that was not.

## Usage

``` r
get_users_by_usernames(
  usernames,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  user_fields = default_user_fields(),
  expansions = NULL
)
```

## Arguments

- usernames:

  A character vector of up to 100 handles, with or without the leading
  "@".

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

- expansions:

  Not used by this endpoint. Accepted so that older code keeps running.

## Value

A tibble with one row per user and the 24 columns described in
[`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
from `created_at` to `user_id`. When no handle is found, the same
columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
users <- get_users_by_usernames(c("Tesla", "XDevelopers"))
} # }
```
