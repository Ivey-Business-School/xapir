# Get Muting

Retrieves a list of Users muted by the authenticated user via the [get
muting endpoint](https://docs.x.com/x-api/users/get-muting).

## Usage

``` r
get_muting(user_fields = default_user_fields())
```

## Arguments

- user_fields:

  `character`, `vector`; the fields to return for each user. Default:
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "public_metrics", "verified", "verified_type", "is_identity_verified", "url")`.

## Value

A tibble containing the muted users

## Examples

``` r
if (FALSE) { # \dontrun{
# Get basic user info for multiple users
muted_users <- get_muting()
} # }
```
