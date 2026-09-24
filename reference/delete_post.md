# Delete Posts

Deletes one or more of the signed-in account's posts by id via the
[delete a post
endpoint](https://docs.x.com/x-api/posts/post-delete-by-post-id). Needs
a user token, so the first call opens a browser window to sign in.

X caps how many posts an account may delete in a 15-minute window, and
the cap is lowest on the Free tier. The ids are therefore deleted in
batches of `batch_size`, with a pause of `sleep_time` seconds between
batches. The defaults, 5 posts then 15 minutes, stay inside the cap on
every tier; raise `batch_size` on a paid tier (up to 50) to go faster.

A post that cannot be deleted does not stop the others: its row records
the API's message and the function carries on.

## Usage

``` r
delete_post(post_ids, sleep_time = 900, batch_size = 5)
```

## Arguments

- post_ids:

  A character vector of the ids of the posts to delete.

- sleep_time:

  Seconds to pause between batches.

- batch_size:

  Number of posts to delete before pausing.

## Value

A tibble with one row per id in `post_ids`: `post_id` (the id),
`deleted` (`TRUE` when the API confirmed the deletion) and `error` (the
error message, or `NA` when the post was deleted).

## Examples

``` r
if (FALSE) { # \dontrun{
delete_post(post_ids = c("1234567890123456789", "1234567890123456790"))
} # }
```
