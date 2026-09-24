# Unlike Post

Removes the signed-in account's like from a post via the [unlike post
endpoint](https://docs.x.com/x-api/users/unlike-post). Needs a user
token, so the first call opens a browser window to sign in. The request
is billed, so the function says what it costs before it sends anything.

## Usage

``` r
unlike_post(post_id)
```

## Arguments

- post_id:

  The id of the post to unlike, as a string.

## Value

Invisibly, the `data` list the API returns, `list(liked = FALSE)`. Stops
with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
unlike_post(post_id = "20")
} # }
```
