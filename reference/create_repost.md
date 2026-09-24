# Create Repost

Reposts a post from the signed-in account via the [repost
endpoint](https://docs.x.com/x-api/posts/repost-post). Needs a user
token, so the first call opens a browser window to sign in.

## Usage

``` r
create_repost(post_id, tweet_id = NULL)
```

## Arguments

- post_id:

  The id of the post to repost, as a string.

- tweet_id:

  Deprecated. Use `post_id`.

## Value

Invisibly, the `data` list the API returns, `list(retweeted = TRUE)`.
Stops with the API's message when the repost is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
create_repost(post_id = "20")
} # }
```
