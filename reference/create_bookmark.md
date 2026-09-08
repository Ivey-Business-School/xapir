# Create Bookmark

Adds a post to the authenticated user’s bookmarks via the [create
bookmark endpoint](https://docs.x.com/x-api/bookmarks/create-bookmark).
The User must match the User context authorizing the request

## Usage

``` r
create_bookmark(username, tweet_id)
```

## Arguments

- username:

  `character`; the name of the account on X without the "@" symbol.

- tweet_id:

  ID of the tweet to be bookmarked.

## Examples

``` r
if (FALSE) { # \dontrun{
create_bookmark(
 username = "Tesla", 
 tweet_id = "1234567890123456789"
)
} # }
```
