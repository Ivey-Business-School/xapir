# Get Owned List

Get a User’s Owned Lists via the [owned list
endpoint](https://docs.x.com/x-api/lists/get-a-users-owned-lists).

## Usage

``` r
get_owned_list(
  username,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  list_fields = c("id", "name", "created_at", "description", "follower_count",
    "member_count", "private")
)
```

## Arguments

- username:

  Username of the account that owns the lists

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- list_fields:

  `character`, `vector`; the fields to return for each list.

## Value

A tibble containing the IDs of the lists and their names, or NULL if
none found.

## Examples

``` r
if (FALSE) { # \dontrun{
lists <- get_owned_list(username = "Tesla")
} # }
```
