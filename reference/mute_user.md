# Mute User on X

Causes the authenticated user to mute a specific User by their ID via
the [mute user endpoint](https://docs.x.com/x-api/users/mute-user).

## Usage

``` r
mute_user(source_username, target_username)
```

## Arguments

- source_username:

  Username of account that will mute someone.

- target_username:

  Username of account that will be muted.

## Examples

``` r
if (FALSE) { # \dontrun{
mute_user(source_username = "myaccount", target_username = "username_to_mute")
} # }
```
