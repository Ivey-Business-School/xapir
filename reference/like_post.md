# Like Post

Likes a post from the signed-in account via the [like post
endpoint](https://docs.x.com/x-api/users/like-post). Needs a user token,
so the first call opens a browser window to sign in. The request is
billed as one interaction, so the function says what it costs before it
sends anything.

## Usage

``` r
like_post(post_id)
```

## Arguments

- post_id:

  The id of the post to like, as a string.

## Value

Invisibly, the `data` list the API returns, `list(liked = TRUE)`. Stops
with the API's message when the like is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
like_post(post_id = "20")
} # }
```
