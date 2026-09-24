# Unmute User

Makes the source account unmute the target account via the [unmute user
endpoint](https://docs.x.com/x-api/users/unmute-user). The source must
be the account that signed in. Needs a user token, so the first call
opens a browser window to sign in.

## Usage

``` r
unmute_user(source_username, target_username)
```

## Arguments

- source_username:

  Username of the account that will unmute, without the "@" symbol. Must
  be the account that signed in.

- target_username:

  Username of the account to unmute, without the "@" symbol.

## Value

Invisibly, the `data` list the API returns, `list(muting = FALSE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
unmute_user(source_username = "myaccount", target_username = "noisyaccount")
} # }
```
