# Follow User

Makes the source account follow the target account (or request to
follow, when the target is protected) via the [follow user
endpoint](https://docs.x.com/x-api/users/follow-user). The source must
be the account that signed in. Needs a user token, so the first call
opens a browser window to sign in.

## Usage

``` r
follow_user(source_username, target_username)
```

## Arguments

- source_username:

  Username of the account that will follow, without the "@" symbol. Must
  be the account that signed in.

- target_username:

  Username of the account to follow, without the "@" symbol.

## Value

Invisibly, the `data` list the API returns,
`list(following = TRUE, pending_follow = FALSE)`. Stops with the API's
message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
follow_user(source_username = "Tesla", target_username = "elonmusk")
} # }
```
