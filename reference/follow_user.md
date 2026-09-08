# Follow User

Causes the User(in the path) to follow, or “request to follow” for
protected Users, the target User via the [follow user
endpoint](https://docs.x.com/x-api/users/follow-user). The User(in the
path) must match the User context authorizing the request

## Usage

``` r
follow_user(source_username, target_username)
```

## Arguments

- source_username:

  Username of account that will follow someone.

- target_username:

  Username of account that will be followed.

## Examples

``` r
if (FALSE) { # \dontrun{
follow_user(source_username = "Tesla", target_username = "elonmusk")
} # }
```
