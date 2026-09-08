# Package index

## Reading posts

One request to the X API, saved whole as a frozen file you unfold later.

- [`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md)
  : Get User Timeline
- [`get_recent_post()`](https://Ivey-Business-School.github.io/xapir/reference/get_recent_post.md)
  : Get Recent Post
- [`get_recent_post_count()`](https://Ivey-Business-School.github.io/xapir/reference/get_recent_post_count.md)
  : Get Recent Post Count
- [`get_post()`](https://Ivey-Business-School.github.io/xapir/reference/get_post.md)
  : Get Post by IDs
- [`get_quote_post()`](https://Ivey-Business-School.github.io/xapir/reference/get_quote_post.md)
  : Retrieve Quote Posts for a Given Post
- [`get_repost()`](https://Ivey-Business-School.github.io/xapir/reference/get_repost.md)
  : Get Repost by ID
- [`get_mentions()`](https://Ivey-Business-School.github.io/xapir/reference/get_mentions.md)
  : Get User Mention Timeline
- [`get_account_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_account_timeline.md)
  : Get User Account Timeline
- [`get_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/get_bookmark.md)
  : Get Bookmark
- [`get_liked_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_liked_posts.md)
  : Get Own Liked Posts
- [`get_liking_users()`](https://Ivey-Business-School.github.io/xapir/reference/get_liking_users.md)
  : Get Liking Users
- [`get_repost_of_me()`](https://Ivey-Business-School.github.io/xapir/reference/get_repost_of_me.md)
  : Get Reposts of Me
- [`iso_8601()`](https://Ivey-Business-School.github.io/xapir/reference/iso_8601.md)
  : Convert a Date or Date-Time to ISO 8601 UTC Format

## Unfolding a pull into tables

The twelve tables of the course’s data model, each from a saved pull.
None of these calls the API.

- [`extract_post()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post.md)
  : Extract Post Data from Timeline
- [`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md)
  : Extract User Data from Timeline
- [`extract_post_media()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_media.md)
  : Extract Media Information from Timeline
- [`extract_post_url()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_url.md)
  : Extract Post URL Information from Timeline
- [`extract_post_mention()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_mention.md)
  : Extract Post Mention Data from Timeline
- [`extract_post_hashtag()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_hashtag.md)
  : Extract Post Hashtag Data from Timeline
- [`extract_post_cashtag()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_cashtag.md)
  : Extract Post Cashtag Data from Timeline
- [`extract_post_context()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_context.md)
  : Extract Post Context Data from Timeline
- [`extract_post_entity_annotation()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_entity_annotation.md)
  : Extract Post Entity Annotation Data from Timeline
- [`extract_post_poll_option()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_poll_option.md)
  : Extract Post Poll Option Information from Timeline
- [`extract_post_place()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_place.md)
  : Extract Post Place and Geo Coordinates from Timeline
- [`extract_post_edited_post_id()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_edited_post_id.md)
  : Extract Post Edited Post ID from Timeline

## Accounts and lists

- [`get_users_by_usernames()`](https://Ivey-Business-School.github.io/xapir/reference/get_users_by_usernames.md)
  : Get Users by Usernames
- [`get_users_by_ids()`](https://Ivey-Business-School.github.io/xapir/reference/get_users_by_ids.md)
  : Get Users by IDs
- [`get_my_user()`](https://Ivey-Business-School.github.io/xapir/reference/get_my_user.md)
  : Get My User
- [`get_blocking()`](https://Ivey-Business-School.github.io/xapir/reference/get_blocking.md)
  : Get Blocking
- [`get_muting()`](https://Ivey-Business-School.github.io/xapir/reference/get_muting.md)
  : Get Muting
- [`get_owned_list()`](https://Ivey-Business-School.github.io/xapir/reference/get_owned_list.md)
  : Get Owned List
- [`get_followed_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_followed_lists.md)
  : Get Followed Lists
- [`get_list_by_id()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_by_id.md)
  : Get List by ID
- [`get_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_member.md)
  : Get List Members
- [`get_trends_by_woeid()`](https://Ivey-Business-School.github.io/xapir/reference/get_trends_by_woeid.md)
  : Get Trends by WOEID

## Writing to X

These act on your own account and need a user token.

- [`create_post()`](https://Ivey-Business-School.github.io/xapir/reference/create_post.md)
  : Create Post on X
- [`delete_post()`](https://Ivey-Business-School.github.io/xapir/reference/delete_post.md)
  : Delete Post(s) on X
- [`create_repost()`](https://Ivey-Business-School.github.io/xapir/reference/create_repost.md)
  : Repost a Post on X
- [`delete_repost()`](https://Ivey-Business-School.github.io/xapir/reference/delete_repost.md)
  : Unrepost a Post on X
- [`create_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/create_bookmark.md)
  : Create Bookmark
- [`delete_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/delete_bookmark.md)
  : Delete Bookmark
- [`hide_reply()`](https://Ivey-Business-School.github.io/xapir/reference/hide_reply.md)
  : Hide Reply on X
- [`follow_user()`](https://Ivey-Business-School.github.io/xapir/reference/follow_user.md)
  : Follow User
- [`unfollow_user()`](https://Ivey-Business-School.github.io/xapir/reference/unfollow_user.md)
  : Unfollow User
- [`mute_user()`](https://Ivey-Business-School.github.io/xapir/reference/mute_user.md)
  : Mute User on X
- [`unmute_user()`](https://Ivey-Business-School.github.io/xapir/reference/unmute_user.md)
  : Unmute User on X
