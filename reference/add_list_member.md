# Add List Member

Adds an account to a list the signed-in account owns via the [add list
member endpoint](https://docs.x.com/x-api/lists/add-list-member). Needs
a user token, so the first call opens a browser window to sign in. The
request is billed, so the function says what it costs before it sends
anything.

Give either `username` or `user_id`, not both. A handle costs one user
read to turn it into an id before the member is added; pass the id when
you already know it and that read is skipped.

## Usage

``` r
add_list_member(list_id, username = NULL, user_id = NULL)
```

## Arguments

- list_id:

  The id of the list, as a string.

- username:

  Username of the account to add, without the "@" symbol.

- user_id:

  The id of the account to add, as a string of digits. When given,
  `username` must be `NULL`.

## Value

Invisibly, the `data` list the API returns, `list(is_member = TRUE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
add_list_member(list_id = "1146654567674912769", username = "Tesla")
add_list_member(list_id = "1146654567674912769", user_id = "13298072")
} # }
```
