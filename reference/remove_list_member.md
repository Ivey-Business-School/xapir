# Remove List Member

Removes an account from a list the signed-in account owns via the
[remove list member
endpoint](https://docs.x.com/x-api/lists/remove-list-member). Needs a
user token, so the first call opens a browser window to sign in. The
request is billed, so the function says what it costs before it sends
anything.

Give either `username` or `user_id`, not both. A handle costs one user
read to turn it into an id before the member is removed.

## Usage

``` r
remove_list_member(list_id, username = NULL, user_id = NULL)
```

## Arguments

- list_id:

  The id of the list, as a string.

- username:

  Username of the account to remove, without the "@" symbol.

- user_id:

  The id of the account to remove, as a string of digits. When given,
  `username` must be `NULL`.

## Value

Invisibly, the `data` list the API returns, `list(is_member = FALSE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
remove_list_member(list_id = "1146654567674912769", username = "Tesla")
} # }
```
