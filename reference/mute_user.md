# Mute User

Makes the source account mute the target account via the [mute user
endpoint](https://docs.x.com/x-api/users/mute-user). The source must be
the account that signed in. Needs a user token, so the first call opens
a browser window to sign in.

## Usage

``` r
mute_user(source_username, target_username)
```

## Arguments

- source_username:

  Username of the account that will mute, without the "@" symbol. Must
  be the account that signed in.

- target_username:

  Username of the account to mute, without the "@" symbol.

## Value

Invisibly, the `data` list the API returns, `list(muting = TRUE)`. Stops
with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
mute_user(source_username = "myaccount", target_username = "noisyaccount")
} # }
```
