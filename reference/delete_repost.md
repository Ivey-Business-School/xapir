# Unrepost a Post on X

Causes the authenticated user to repost a specific Post by its ID. The
User in the path must match the User context authorizing the request.
This is done via the [retweet
endpoint](https://docs.x.com/x-api/posts/unrepost-post).

## Usage

``` r
delete_repost(tweet_id)
```

## Arguments

- tweet_id:

  The ID of the post to be unreposted.

## Examples

``` r
if (FALSE) { # \dontrun{
delete_repost(tweet_id = "20")
} # }
```
