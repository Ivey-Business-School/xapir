# Create Post on X

Causes the User to create a Post under the authorized account via the
[create a post
endpoint](https://docs.x.com/x-api/posts/creation-of-a-post).

## Usage

``` r
create_post(
  text,
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

  The tweet text (max 280 characters)

- for_super_followers_only:

  Exclusive Tweet for super followers.

- geo:

  Place ID being attached to the Tweet for geo location.

- media:

  Media information being attached to created Tweet. This is mutually
  exclusive from Quote Tweet Id, Poll, and Card URI.

- nullcast:

  Nullcasted (promoted-only) Posts do not appear in the public timeline
  and are not served to followers.

- poll:

  Poll options for a Tweet with a poll. This is mutually exclusive from
  Media, Quote Tweet Id, and Card URI.

- reply:

  Tweet information of the Tweet being replied to.

- reply_settings:

  Settings to indicate who can reply to the Tweet.

## Examples

``` r
if (FALSE) { # \dontrun{
create_post(text = "Hello, world!")
} # }
```
