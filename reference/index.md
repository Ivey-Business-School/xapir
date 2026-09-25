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

## Accounts and follows

Profiles by handle, id or search, and who follows, is followed by,
reposts, blocks or mutes. Each returns the 24 user columns of
[`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md).

- [`get_users_by_usernames()`](https://Ivey-Business-School.github.io/xapir/reference/get_users_by_usernames.md)
  : Get Users by Usernames
- [`get_users_by_ids()`](https://Ivey-Business-School.github.io/xapir/reference/get_users_by_ids.md)
  : Get Users by IDs
- [`search_users()`](https://Ivey-Business-School.github.io/xapir/reference/search_users.md)
  : Search Users
- [`get_followers()`](https://Ivey-Business-School.github.io/xapir/reference/get_followers.md)
  : Get Followers
- [`get_following()`](https://Ivey-Business-School.github.io/xapir/reference/get_following.md)
  : Get Following
- [`get_reposted_by()`](https://Ivey-Business-School.github.io/xapir/reference/get_reposted_by.md)
  : Get Reposted By
- [`get_blocking()`](https://Ivey-Business-School.github.io/xapir/reference/get_blocking.md)
  : Get Blocking
- [`get_muting()`](https://Ivey-Business-School.github.io/xapir/reference/get_muting.md)
  : Get Muting

## Lists

Lists an account owns, follows, pinned or was added to, and a list’s
members, followers and posts.

- [`get_owned_list()`](https://Ivey-Business-School.github.io/xapir/reference/get_owned_list.md)
  : Get Owned Lists
- [`get_followed_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_followed_lists.md)
  : Get Followed Lists
- [`get_list_by_id()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_by_id.md)
  : Get List by ID
- [`get_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_member.md)
  : Get List Members
- [`get_list_followers()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_followers.md)
  : Get List Followers
- [`get_list_memberships()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_memberships.md)
  : Get List Memberships
- [`get_list_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_posts.md)
  : Get List Posts
- [`get_pinned_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_pinned_lists.md)
  : Get Pinned Lists

## Search the archive

Every public post back to 2006. Needs pay-per-use or Enterprise access.

- [`get_all_post()`](https://Ivey-Business-School.github.io/xapir/reference/get_all_post.md)
  : Get All Posts
- [`get_all_post_count()`](https://Ivey-Business-School.github.io/xapir/reference/get_all_post_count.md)
  : Get All Post Count

## Spaces, communities and news

Audio spaces, communities, the news stories X builds from posts, and
trending topics by place.

- [`get_spaces()`](https://Ivey-Business-School.github.io/xapir/reference/get_spaces.md)
  : Get Spaces by IDs or by Creator IDs
- [`search_spaces()`](https://Ivey-Business-School.github.io/xapir/reference/search_spaces.md)
  : Search Spaces
- [`get_space_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_space_posts.md)
  : Get Space Posts
- [`get_community()`](https://Ivey-Business-School.github.io/xapir/reference/get_community.md)
  : Get Community by ID
- [`search_communities()`](https://Ivey-Business-School.github.io/xapir/reference/search_communities.md)
  : Search Communities
- [`search_news()`](https://Ivey-Business-School.github.io/xapir/reference/search_news.md)
  : Search News
- [`get_news()`](https://Ivey-Business-School.github.io/xapir/reference/get_news.md)
  : Get News Story by ID
- [`get_trends_by_woeid()`](https://Ivey-Business-School.github.io/xapir/reference/get_trends_by_woeid.md)
  : Get Trends by WOEID

## Your own account

What your project has read and spent, how your posts performed, your
trends and your profile. All need a sign-in except usage.

- [`get_my_user()`](https://Ivey-Business-School.github.io/xapir/reference/get_my_user.md)
  : Get My User
- [`get_usage()`](https://Ivey-Business-School.github.io/xapir/reference/get_usage.md)
  : Get Usage
- [`get_spend()`](https://Ivey-Business-School.github.io/xapir/reference/get_spend.md)
  : Get Spend
- [`get_usage_credits()`](https://Ivey-Business-School.github.io/xapir/reference/get_usage_credits.md)
  : Get Usage Credits
- [`get_post_analytics()`](https://Ivey-Business-School.github.io/xapir/reference/get_post_analytics.md)
  : Get Post Analytics
- [`get_personalized_trends()`](https://Ivey-Business-School.github.io/xapir/reference/get_personalized_trends.md)
  : Get Personalized Trends

## Writing to X

These act on your own account and need a user token. Each prints its
price before the request.

### Posts and media

- [`create_post()`](https://Ivey-Business-School.github.io/xapir/reference/create_post.md)
  : Create Post
- [`upload_media()`](https://Ivey-Business-School.github.io/xapir/reference/upload_media.md)
  : Upload Media
- [`delete_post()`](https://Ivey-Business-School.github.io/xapir/reference/delete_post.md)
  : Delete Posts
- [`create_repost()`](https://Ivey-Business-School.github.io/xapir/reference/create_repost.md)
  : Create Repost
- [`delete_repost()`](https://Ivey-Business-School.github.io/xapir/reference/delete_repost.md)
  : Delete Repost

### Interactions

- [`like_post()`](https://Ivey-Business-School.github.io/xapir/reference/like_post.md)
  : Like Post
- [`unlike_post()`](https://Ivey-Business-School.github.io/xapir/reference/unlike_post.md)
  : Unlike Post
- [`follow_user()`](https://Ivey-Business-School.github.io/xapir/reference/follow_user.md)
  : Follow User
- [`unfollow_user()`](https://Ivey-Business-School.github.io/xapir/reference/unfollow_user.md)
  : Unfollow User

### Lists

- [`create_list()`](https://Ivey-Business-School.github.io/xapir/reference/create_list.md)
  : Create List
- [`update_list()`](https://Ivey-Business-School.github.io/xapir/reference/update_list.md)
  : Update List
- [`delete_list()`](https://Ivey-Business-School.github.io/xapir/reference/delete_list.md)
  : Delete List
- [`add_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/add_list_member.md)
  : Add List Member
- [`remove_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/remove_list_member.md)
  : Remove List Member
- [`follow_list()`](https://Ivey-Business-School.github.io/xapir/reference/follow_list.md)
  : Follow List
- [`unfollow_list()`](https://Ivey-Business-School.github.io/xapir/reference/unfollow_list.md)
  : Unfollow List
- [`pin_list()`](https://Ivey-Business-School.github.io/xapir/reference/pin_list.md)
  : Pin List
- [`unpin_list()`](https://Ivey-Business-School.github.io/xapir/reference/unpin_list.md)
  : Unpin List

### Bookmarks

- [`create_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/create_bookmark.md)
  : Create Bookmark
- [`delete_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/delete_bookmark.md)
  : Delete Bookmark

### Moderation

- [`hide_reply()`](https://Ivey-Business-School.github.io/xapir/reference/hide_reply.md)
  : Hide Reply
- [`mute_user()`](https://Ivey-Business-School.github.io/xapir/reference/mute_user.md)
  : Mute User
- [`unmute_user()`](https://Ivey-Business-School.github.io/xapir/reference/unmute_user.md)
  : Unmute User
- [`block_user()`](https://Ivey-Business-School.github.io/xapir/reference/block_user.md)
  : Block User
- [`unblock_user()`](https://Ivey-Business-School.github.io/xapir/reference/unblock_user.md)
  : Unblock User
