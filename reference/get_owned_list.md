# Get Owned Lists

Retrieves the lists a user owns via the [owned lists
endpoint](https://docs.x.com/x-api/lists/get-a-users-owned-lists).

Give either `username` or `user_id`, not both. A `username` costs one
user read to turn the handle into an id before the lists are read. When
you already know the account's id, pass `user_id` and that read is
skipped.

## Usage

``` r
get_owned_list(
  username = NULL,
  user_id = NULL,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  list_fields = c("id", "name", "created_at", "description", "follower_count",
    "member_count", "private", "owner_id")
)
```

## Arguments

- username:

  `character`; the name of the account on X without the "@" symbol.

- user_id:

  `character`; the account's X user id, as a string of digits. When
  given, the handle lookup is skipped and `username` must be `NULL`.

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

A tibble with one row per list: `list_id`, `list_name`, `description`,
`created_at` (POSIXct, UTC), `follower_count`, `member_count`, `private`
and `owner_id`. A user with no lists gives the same columns with no
rows.

## Examples

``` r
if (FALSE) { # \dontrun{
lists <- get_owned_list(username = "Tesla")

# The same lists by id, with no user read for the handle
lists <- get_owned_list(user_id = "13298072")
} # }
```
