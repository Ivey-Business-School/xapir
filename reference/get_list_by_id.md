# Get List by ID

Retrieves the details of a specific List by its ID via the [Get List by
ID endpoint](https://docs.x.com/x-api/lists/get-list-by-id).

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

  The ID of the List to retrieve.

- bearer_token:

  A string containing the bearer token for authenticating with the X
  API. By default, this argument retrieves the token from the
  environment variable `X_BEARER_TOKEN` (via
  `Sys.getenv("X_BEARER_TOKEN")`). Adding your bearer token to your
  `.Renviron` file keeps it out of your scripts and available in every
  session.

- list_fields:

  Character vector of fields to include (default common fields).

## Value

A tibble with list details (one row), or NULL if there's an error.

## Examples

``` r
if (FALSE) { # \dontrun{
lst <- get_list_by_id(list_id = "1146654567674912769")
} # }
```
