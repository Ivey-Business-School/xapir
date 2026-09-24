# Create Post

Publishes a post from the signed-in account via the [create a post
endpoint](https://docs.x.com/x-api/posts/create-post). Needs a user
token, so the first call opens a browser window to sign in.

The plain arguments (`media_ids`, `quote_post_id`, `reply_to_post_id`,
`poll_options`, ...) cover the everyday cases. The list arguments
(`media`, `poll`, `reply`, `geo`) take the API's own nested objects for
anything the plain ones do not reach, such as tagging users in a photo.
Give one or the other for each part, not both.

## Usage

``` r
create_post(
  text,
  media_ids = NULL,
  quote_post_id = NULL,
  reply_to_post_id = NULL,
  poll_options = NULL,
  poll_duration_minutes = 1440,
  community_id = NULL,
  paid_partnership = FALSE,
  share_with_followers = FALSE,
  for_super_followers_only = FALSE,
  geo = NULL,
  media = NULL,
  nullcast = FALSE,
  poll = NULL,
  reply = NULL,
  reply_settings = NULL
)
```

## Arguments

- text:

  The text of the post. The maximum length depends on the account's tier
  (280 characters on a standard account, longer on Premium). The API
  refuses text that is too long and the error says so.

- media_ids:

  Ids of media uploaded with
  [`upload_media()`](https://Ivey-Business-School.github.io/xapir/reference/upload_media.md),
  as strings: up to 4 photos, or 1 GIF, or 1 video. Cannot be combined
  with a poll.

- quote_post_id:

  The id of a post to quote, as a string.

- reply_to_post_id:

  The id of the post to reply to, as a string.

- poll_options:

  Two to four choices for a poll, each 1 to 25 characters. Cannot be
  combined with media.

- poll_duration_minutes:

  How long the poll runs, 5 to 10,080 minutes (a week). The default is a
  day.

- community_id:

  The id of an X community to post in, as a string.

- paid_partnership:

  `TRUE` to label the post as a paid partnership.

- share_with_followers:

  `TRUE` to share a super-followers-only post with all followers.

- for_super_followers_only:

  `TRUE` to show the post only to super followers.

- geo:

  A list with a `place_id`, to attach a place to the post.

- media:

  A list with `media_ids` (and optionally `tagged_user_ids`), the API's
  own media object, for what `media_ids` does not cover.

- nullcast:

  `TRUE` for a promoted-only post that does not appear in the public
  timeline.

- poll:

  A list with `options` and `duration_minutes`, the API's own poll
  object, for what `poll_options` does not cover.

- reply:

  A list with `in_reply_to_tweet_id` (and optionally
  `exclude_reply_user_ids`), the API's own reply object, for what
  `reply_to_post_id` does not cover.

- reply_settings:

  Who can reply: `"following"`, `"mentionedUsers"`, `"subscribers"` or
  `"verified"`. Leave `NULL` to let everyone reply.

## Value

Invisibly, the `data` list the API returns, with the new post's `id` and
`text`. Stops with the API's message when the post is refused.

## Details

A post is billed per request, and a post whose text contains a link
costs more: the pricing page lists "Post: Create" at \$0.015 and "Post:
Create (with URL)" at \$0.200, more than thirteen times as much. The
cost line printed before the request shows which rate applies, so check
it before posting a batch with links. A link is anything that looks like
`https://...`, `http://...` or `www....`.

Upload a photo, GIF or video with
[`upload_media()`](https://Ivey-Business-School.github.io/xapir/reference/upload_media.md)
first and pass the id it returns as `media_ids`. A post can carry up to
4 photos, 1 GIF or 1 video, and the docs say media cannot be combined
with a poll.

The docs also say quote posts (`quote_post_id`) need an Enterprise plan;
on a pay-per-use account the API refuses them with its own message.

## Examples

``` r
if (FALSE) { # \dontrun{
new_post <- create_post(text = "Hello, world!")
new_post$id

# A photo, uploaded first
media_id <- upload_media("chart.png", alt_text = "Sales by month")
create_post("Our year so far", media_ids = media_id)

# A reply, a quote, a poll
create_post("Agreed!", reply_to_post_id = new_post$id)
create_post("Worth a read", quote_post_id = "1234567890123456789")
create_post("Which one?", poll_options = c("This", "That"),
            poll_duration_minutes = 60)
} # }
```
