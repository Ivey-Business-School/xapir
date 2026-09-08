# Hide Reply on X

Hides or unhides a reply to a conversation owned by the authenticated
user via the [hide reply
endpoint](https://docs.x.com/x-api/posts/hide-reply).

## Usage

``` r
hide_reply(reply_id, hidden = TRUE)
```

## Arguments

- reply_id:

  The ID of the reply to be hidden. Must be a reply to a post authored
  by the authenticating user.

- hidden:

  Indicates whether the reply should be hidden (TRUE) or unhidden
  (FALSE). Defaults to TRUE.

## Examples

``` r
if (FALSE) { # \dontrun{
# Hide a reply
hide_reply(reply_id = "1234567890123456789")

# Unhide a reply
hide_reply(reply_id = "1234567890123456789", hidden = FALSE)
} # }
```
