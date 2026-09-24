# Delete Repost

Removes the signed-in account's repost of a post via the [unrepost
endpoint](https://docs.x.com/x-api/posts/unrepost-post). Needs a user
token, so the first call opens a browser window to sign in.

## Usage

``` r
delete_repost(post_id, tweet_id = NULL)
```

## Arguments

- post_id:

  The id of the post whose repost to remove, as a string.

- tweet_id:

  Deprecated. Use `post_id`.

## Value

Invisibly, the `data` list the API returns, `list(retweeted = FALSE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
delete_repost(post_id = "20")
} # }
```
