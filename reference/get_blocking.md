# Get Blocking

Retrieves a list of Users blocked by the specified User ID via the [get
blocking endpoint](https://docs.x.com/x-api/users/get-blocking).

## Usage

``` r
get_blocking(
  user_fields = default_user_fields(),
  max_results = 100,
  pagination_token = NULL
)
```

## Arguments

- user_fields:

  `character`, `vector`; the fields to return for each user. Default:
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "public_metrics", "verified", "verified_type", "is_identity_verified", "url")`.

- max_results:

  The maximum number of results to return per page. Must be between 1
  and 1000. Default is 100.

- pagination_token:

  Contains either a next_token or previous_token value.

## Value

A tibble containing information on blocked users

## Examples

``` r
if (FALSE) { # \dontrun{
blocked_users <- get_blocking()
} # }
```
