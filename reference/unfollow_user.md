# Unfollow User

Causes the source User to unfollow the target User via the [unfollow
user endpoint](https://docs.x.com/x-api/users/unfollow-user). The source
User must match the User context authorizing the request

## Usage

``` r
unfollow_user(source_username, target_username)
```

## Arguments

- source_username:

  Username of account that will unfollow someone.

- target_username:

  Username of account that will be unfollowed.

## Examples

``` r
if (FALSE) { # \dontrun{
unfollow_user(source_username = "Tesla", target_username = "elonmusk")
} # }
```
