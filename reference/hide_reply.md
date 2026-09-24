# Hide Reply

Hides, or unhides, a reply to one of the signed-in account's posts via
the [hide reply endpoint](https://docs.x.com/x-api/posts/hide-reply).
Needs a user token, so the first call opens a browser window to sign in.

## Usage

``` r
hide_reply(reply_id, hidden = TRUE)
```

## Arguments

- reply_id:

  The id of the reply, as a string. It must be a reply to a post by the
  account that signed in.

- hidden:

  `TRUE` (the default) hides the reply, `FALSE` shows it again.

## Value

Invisibly, the `data` list the API returns, `list(hidden = TRUE)` or
`list(hidden = FALSE)`. Stops with the API's message when the request is
refused.

## Examples

``` r
if (FALSE) { # \dontrun{
# Hide a reply
hide_reply(reply_id = "1234567890123456789")

# Show it again
hide_reply(reply_id = "1234567890123456789", hidden = FALSE)
} # }
```
