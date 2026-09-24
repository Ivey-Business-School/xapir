# Delete List

Deletes a list the signed-in account owns via the [delete list
endpoint](https://docs.x.com/x-api/lists/delete-list). Needs a user
token, so the first call opens a browser window to sign in. The request
is billed, so the function says what it costs before it sends anything.

## Usage

``` r
delete_list(list_id)
```

## Arguments

- list_id:

  The id of the list to delete, as a string.

## Value

Invisibly, the `data` list the API returns, `list(deleted = TRUE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
delete_list(list_id = "1146654567674912769")
} # }
```
