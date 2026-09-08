# Get List Members

Returns a list of Users that are members of a List by the provided List
ID via the [get list
members](https://docs.x.com/x-api/users/returns-user-objects-that-are-members-of-a-list-by-the-provided-list-id).

## Usage

``` r
get_list_member(
  list_id,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  user_fields = default_user_fields()
)
```

## Arguments

- list_id:

  A string representing the unique ID of the list.

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

## Value

A list containing the API response.

## Examples

``` r
if (FALSE) { # \dontrun{
list_members <- get_list_members(list_ID)
} # }
```
