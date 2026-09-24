# Get List Memberships

Returns the lists a user has been added to via the [get list memberships
endpoint](https://docs.x.com/x-api/users/get-list-memberships). Every
list returned is billed (US\$0.005 each in September 2026), so the
function says what the call can cost before it reads anything, and stops
reading at `max_lists`.

Give either `username` or `user_id`, not both. A `username` costs one
user read to turn the handle into an id before the lists are read. When
you already know the account's id, pass `user_id` and that read is
skipped.

## Usage

``` r
get_list_memberships(
  username = NULL,
  user_id = NULL,
  max_results = 100,
  max_lists = 100,
  pagination_token = NULL,
  sleep_time = 0,
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

- max_results:

  `numeric`; the number of lists per API call, between 1 and 100. The
  function stops before any request if the value is outside that range.

- max_lists:

  `numeric`; the most lists to read across all pages. Reading stops once
  this many have been returned. Default 100.

- pagination_token:

  A string used to navigate backward through result pages. The X API
  provides this token when more results are available. Typically, you
  won\<80\>\<99\>t need to set this manually as the function handles it,
  but you can supply a pagination_token from a previous response to
  continue retrieving results beyond the last page, if desired.

- sleep_time:

  Seconds to pause between pages, `0` by default. A pause is optional:
  rate limits are handled for you. A 429 or a 5xx response is retried up
  to three times, waiting as long as the API asks. Any other error stops
  at once with the API's own message, so a mistyped handle or a bad
  token fails in seconds.

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
and `owner_id`. A user on no lists gives the same columns with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
lists <- get_list_memberships(username = "XDevelopers")

# The same lists by id, with no user read for the handle
lists <- get_list_memberships(user_id = "2244994945")
} # }
```
