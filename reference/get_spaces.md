# Get Spaces by IDs or by Creator IDs

Retrieves up to 100 spaces by their ids via the [get spaces by IDs
endpoint](https://docs.x.com/x-api/spaces/get-spaces-by-ids), or every
space created by up to 100 users via the [get spaces by creator IDs
endpoint](https://docs.x.com/x-api/spaces/get-spaces-by-creator-ids).
Give exactly one of `space_ids` and `user_ids`. Every space returned is
billed, so the function says what the call can cost before it reads
anything. An app bearer token is enough.

An id the API cannot find does not stop the call: the spaces it did find
are returned, and one warning names each id that was not.

## Usage

``` r
get_spaces(
  space_ids = NULL,
  user_ids = NULL,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  space_fields = default_space_fields()
)
```

## Arguments

- space_ids:

  A character vector of up to 100 space ids, each a short string of
  letters and digits such as `"1DXxyRYNejbKM"`.

- user_ids:

  A character vector of up to 100 user ids, each a string of digits.
  Keep ids as text: as numbers they lose digits. Returns the spaces
  those users created, so one user id can give several rows.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- space_fields:

  `character`, `vector`; the fields to return for each space. The
  default asks for everything the table reads.

## Value

A tibble with one row per space: `space_id`, `title`, `state` (`"live"`,
`"scheduled"` or `"ended"`), `creator_id`, `created_at`,
`scheduled_start`, `started_at` and `ended_at` (POSIXct, UTC), `lang`,
`is_ticketed`, `participant_count`, `subscriber_count`, `host_ids` and
`speaker_ids` (list-columns, one character vector of user ids per row).
When no space is found, the same columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
spaces <- get_spaces(space_ids = c("1DXxyRYNejbKM", "1nAKErYNqlpxL"))
spaces <- get_spaces(user_ids = c("783214", "2244994945"))
} # }
```
