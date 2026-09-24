# Delete Bookmark

Removes a post from the signed-in account's bookmarks via the [delete
bookmark endpoint](https://docs.x.com/x-api/bookmarks/delete-bookmark).
Needs a user token, so the first call opens a browser window to sign in.

## Usage

``` r
delete_bookmark(post_id, username = NULL, tweet_id = NULL)
```

## Arguments

- post_id:

  The id of the post to remove from the bookmarks, as a string.

- username:

  Deprecated and ignored. Bookmarks always belong to the account that
  signed in.

- tweet_id:

  Deprecated. Use `post_id`.

## Value

Invisibly, the `data` list the API returns, `list(bookmarked = FALSE)`.
Stops with the API's message when the request is refused.

## Examples

``` r
if (FALSE) { # \dontrun{
delete_bookmark(post_id = "1234567890123456789")
} # }
```
