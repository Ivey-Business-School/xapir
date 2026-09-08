# Get Users by IDs

Retrieves details of multiple Users by their IDs via the [get users by
IDs endpoint](https://docs.x.com/x-api/users/get-users-by-ids).

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

  A list of User IDs. Up to 100 comma-separated User IDs can be looked
  up using this endpoint.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- user_fields:

  `character`, `vector`; the fields to return for each user. Default:
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "public_metrics", "verified", "verified_type", "is_identity_verified", "url")`.

- expansions:

  Not used by this endpoint. Accepted so that older code keeps running.

## Value

A tibble containing the user information

## Examples

``` r
if (FALSE) { # \dontrun{
# Get basic user info for multiple users
get_users_by_ids(
user_ids = c("783214", "2244994945")
)
} # }
```
