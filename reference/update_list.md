# Update List

Changes the name, description or privacy of a list the signed-in account
owns via the [update list
endpoint](https://docs.x.com/x-api/lists/update-list). Only the
arguments given are sent; the rest stay as they are. Needs a user token,
so the first call opens a browser window to sign in. The request is
billed, so the function says what it costs before it sends anything.

## Usage

``` r
update_list(list_id, name = NULL, description = NULL, private = NULL)
```

## Arguments

- list_id:

  The id of the list, as a string.

- name:

  A new name, 1 to 25 characters, or `NULL` to keep the old one.

- description:

  A new description, up to 100 characters, or `NULL`.

- private:

  `TRUE` or `FALSE` to change the privacy, or `NULL`.

## Value

Invisibly, the `data` list the API returns, `list(updated = TRUE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
update_list(list_id = "1146654567674912769", name = "EV makers 2026")
update_list(list_id = "1146654567674912769", private = TRUE)
} # }
```
