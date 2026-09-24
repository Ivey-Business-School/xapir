# Get List by ID

Retrieves the details of one list by its id via the [get list by ID
endpoint](https://docs.x.com/x-api/lists/get-list-by-id). An id the API
cannot find stops the call with the API's reason.

## Usage

``` r
get_list_by_id(
  list_id,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  list_fields = c("id", "name", "description", "created_at", "follower_count",
    "member_count", "owner_id", "private")
)
```

## Arguments

- list_id:

  The list's id, as a string of digits.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- list_fields:

  `character`, `vector`; the fields to return for the list.

## Value

A tibble with one row: `list_id`, `list_name`, `description`,
`created_at` (POSIXct, UTC), `follower_count`, `member_count`, `private`
and `owner_id`.

## Examples

``` r
if (FALSE) { # \dontrun{
lst <- get_list_by_id(list_id = "1146654567674912769")
} # }
```
