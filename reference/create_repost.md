# Repost a Post on X

Causes the User (in the path) to repost the specified Post. The User in
the path must match the User context authorizing the request. This is
done via the [retweet
endpoint](https://docs.x.com/x-api/posts/repost-post).

## Usage

``` r
create_repost(tweet_id)
```

## Arguments

- tweet_id:

  The ID of the post to be reposted.

## Examples

``` r
if (FALSE) { # \dontrun{
create_repost(tweet_id = "20")
} # }
```
