# Pin List

Pins a list to the top of the signed-in account's lists via the [pin
list endpoint](https://docs.x.com/x-api/users/pin-list). An account can
pin up to five lists. Needs a user token, so the first call opens a
browser window to sign in. The request is billed, so the function says
what it costs before it sends anything.

## Usage

``` r
pin_list(list_id)
```

## Arguments

- list_id:

  The id of the list to pin, as a string.

## Value

Invisibly, the `data` list the API returns, `list(pinned = TRUE)`. Stops
with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
pin_list(list_id = "1146654567674912769")
} # }
```
