# Unmute User on X

Causes the authenticated user to unmute a specific user by their ID via
the [unmute user endpoint](https://docs.x.com/x-api/users/unmute-user).

## Usage

``` r
unmute_user(source_username, target_username)
```

## Arguments

- source_username:

  Username of account that will unmute someone.

- target_username:

  Username of account that will be unmuted.

## Examples

``` r
if (FALSE) { # \dontrun{
unmute_user(source_username = "myaccount", target_username = "username_to_mute")
} # }
```
