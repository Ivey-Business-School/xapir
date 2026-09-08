# Delete Bookmark

Removes a Post from the authenticated user’s Bookmarks by its ID via the
[delete bookmark
endpoint](https://docs.x.com/x-api/bookmarks/delete-bookmark). The User
must match the User context authorizing the request

## Usage

``` r
delete_bookmark(username, tweet_id)
```

## Arguments

- username:

  `character`; the name of the account on X without the "@" symbol.

- tweet_id:

  ID of the tweet to be bookmarked.

## Examples

``` r
if (FALSE) { # \dontrun{
delete_bookmark(
 username = "Tesla", 
 tweet_id = "1234567890123456789"
)
} # }
```
