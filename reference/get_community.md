# Get Community by ID

Retrieves the details of one community by its id via the [get community
by ID
endpoint](https://docs.x.com/x-api/communities/get-community-by-id).
Every community returned is billed, so the function says what the call
can cost before it reads anything. An app bearer token is enough. An id
the API cannot find stops the call with the API's reason.

## Usage

``` r
get_community(
  community_id,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  community_fields = default_community_fields()
)
```

## Arguments

- community_id:

  The community's id, as a string of digits. Keep ids as text: as
  numbers they lose digits.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- community_fields:

  `character`, `vector`; the fields to return for the community. The
  default asks for everything the table reads.

## Value

A tibble with one row: `community_id`, `name`, `description`, `access`
(`"Public"` or `"Closed"`), `join_policy` (`"Open"`,
`"RestrictedJoinRequestsDisabled"`, ...), `member_count` and
`created_at` (POSIXct, UTC).

## Examples

``` r
if (FALSE) { # \dontrun{
community <- get_community("1493446837214187523")
} # }
```
