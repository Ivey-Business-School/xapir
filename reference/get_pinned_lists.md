# Get Pinned Lists

Retrieves the lists the signed-in account has pinned via the [get pinned
lists endpoint](https://docs.x.com/x-api/users/get-users-pinned-lists).
Needs a user token, so the first call opens a browser window to sign in.
An account can pin at most five lists and every list returned is billed,
so the function says what the call can cost before it reads anything.

## Usage

``` r
get_pinned_lists(
  list_fields = c("id", "name", "created_at", "description", "follower_count",
    "member_count", "private", "owner_id")
)
```

## Arguments

- list_fields:

  `character`, `vector`; the fields to return for each list.

## Value

A tibble with one row per pinned list: `list_id`, `list_name`,
`description`, `created_at` (POSIXct, UTC), `follower_count`,
`member_count`, `private` and `owner_id`. When nothing is pinned, the
same columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
pinned <- get_pinned_lists()
} # }
```
