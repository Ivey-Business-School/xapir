# Search Spaces

Finds live or scheduled spaces whose title matches a search via the
[search spaces endpoint](https://docs.x.com/x-api/spaces/search-spaces).
Every space returned is billed, so the function says what the call can
cost before it reads anything. An app bearer token is enough.

## Usage

``` r
search_spaces(
  query,
  state = "all",
  max_results = 100,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  space_fields = default_space_fields()
)
```

## Arguments

- query:

  One search string, matched against space titles.

- state:

  Which spaces to return: `"live"`, `"scheduled"` or `"all"` (the
  default).

- max_results:

  The most spaces to return, between 1 and 100. Default 100. The
  function stops before any request if the value is outside that range.

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

A tibble with one row per space and the columns described in
[`get_spaces()`](https://Ivey-Business-School.github.io/xapir/reference/get_spaces.md):
`space_id`, `title`, `state`, `creator_id`, `created_at`,
`scheduled_start`, `started_at`, `ended_at` (POSIXct, UTC), `lang`,
`is_ticketed`, `participant_count`, `subscriber_count`, `host_ids` and
`speaker_ids` (list-columns). When nothing matches, the same columns
with no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
live <- search_spaces("marketing", state = "live", max_results = 20)
} # }
```
