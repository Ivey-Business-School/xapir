# Delete Post(s) on X

Delete one or more Posts (in the path) by ID via the [delete a post
endpoint](https://docs.x.com/x-api/posts/post-delete-by-post-id).

## Usage

``` r
delete_post(post_ids, sleep_time = 900)
```

## Arguments

- post_ids:

  A character vector of post IDs that are to be deleted from your X
  account

- sleep_time:

  Seconds to pause between deletions when more than one post id is
  given.

## Value

A tibble containing the requested post IDs to delete, whether they were
deleted successfully, and any error messages

## Examples

``` r
if (FALSE) { # \dontrun{
delete_post(post_ids =  c("post_id1", "post_id2", "post_id3"))
} # }
```
