# Get My User

Returns details about the authenticated User via the [get my user
endpoint](https://docs.x.com/x-api/users/get-my-user).

## Usage

``` r
get_my_user(user_fields = default_user_fields())
```

## Arguments

- user_fields:

  `character`, `vector`; the fields to return for each user. Default:
  `c("created_at", "description", "protected", "entities", "location", "profile_image_url", "public_metrics", "verified", "verified_type", "is_identity_verified", "url")`.

## Value

A tibble containing the authenticated user's information and any
expansions.

## Examples

``` r
if (FALSE) { # \dontrun{
my_user <- get_my_user()
} # }
```
