# Unblock User

Removes a block from the signed-in account via the [unblock user
endpoint](https://docs.x.com/x-api/users/unblock-user). Needs a user
token, so the first call opens a browser window to sign in. The request
is billed, so the function says what it costs before it sends anything.

Give either `target_username` or `target_user_id`, not both. A handle
costs one user read to turn it into an id before the request is sent.

The docs list this endpoint as Enterprise only, so on a pay-per-use tier
the API may refuse it. The function passes the API's message on.

## Usage

``` r
unblock_user(target_username = NULL, target_user_id = NULL)
```

## Arguments

- target_username:

  Username of the account to unblock, without the "@" symbol.

- target_user_id:

  The id of the account to unblock, as a string of digits. When given,
  `target_username` must be `NULL`.

## Value

Invisibly, the `data` list the API returns, `list(blocking = FALSE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
unblock_user(target_username = "spammer")
} # }
```
