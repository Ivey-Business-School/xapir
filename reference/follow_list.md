# Follow List

Makes the signed-in account follow a list via the [follow list
endpoint](https://docs.x.com/x-api/users/follow-list). Needs a user
token, so the first call opens a browser window to sign in. The request
is billed, so the function says what it costs before it sends anything.

## Usage

``` r
follow_list(list_id)
```

## Arguments

- list_id:

  The id of the list to follow, as a string.

## Value

Invisibly, the `data` list the API returns, `list(following = TRUE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
follow_list(list_id = "1146654567674912769")
} # }
```
