# Unfollow User

Makes the source account unfollow the target account via the [unfollow
user endpoint](https://docs.x.com/x-api/users/unfollow-user). The source
must be the account that signed in. Needs a user token, so the first
call opens a browser window to sign in.

## Usage

``` r
unfollow_user(source_username, target_username)
```

## Arguments

- source_username:

  Username of the account that will unfollow, without the "@" symbol.
  Must be the account that signed in.

- target_username:

  Username of the account to unfollow, without the "@" symbol.

## Value

Invisibly, the `data` list the API returns, `list(following = FALSE)`.
Stops with the API's message when the request is refused, for example
when the source does not follow the target.

## Examples

``` r
if (FALSE) { # \dontrun{
unfollow_user(source_username = "Tesla", target_username = "elonmusk")
} # }
```
