# Unfollow List

Makes the signed-in account unfollow a list via the [unfollow list
endpoint](https://docs.x.com/x-api/users/unfollow-list). Needs a user
token, so the first call opens a browser window to sign in. The request
is billed, so the function says what it costs before it sends anything.

## Usage

``` r
unfollow_list(list_id)
```

## Arguments

- list_id:

  The id of the list to unfollow, as a string.

## Value

Invisibly, the `data` list the API returns, `list(following = FALSE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
unfollow_list(list_id = "1146654567674912769")
} # }
```
