# Get Followed Lists

Retrieves the Lists followed by a given User via the [get followed lists
endpoint](https://docs.x.com/x-api/users/get-followed-lists).

## Usage

``` r
get_followed_lists(
  username,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  list_fields = c("id", "name", "created_at", "description", "follower_count",
    "member_count", "private")
)
```

## Arguments

- username:

  Username of the account whose followed lists are being retrieved.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- list_fields:

  Character vector of list fields to include in the response. Defaults
  to commonly useful fields.

## Value

A tibble containing the IDs of the followed lists and their metadata, or
NULL if none are found.

## Examples

``` r
if (FALSE) { # \dontrun{
lists <- get_followed_lists(username = "XDevelopers")
} # }
```
