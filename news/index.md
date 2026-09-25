# Changelog

## xapir 0.2.0

Install it with `pak::pak("Ivey-Business-School/xapir@v0.2.0")`.

### Breaking changes

- `tweet_id` is now `post_id` in
  [`create_repost()`](https://Ivey-Business-School.github.io/xapir/reference/create_repost.md),
  [`delete_repost()`](https://Ivey-Business-School.github.io/xapir/reference/delete_repost.md),
  [`create_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/create_bookmark.md)
  and
  [`delete_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/delete_bookmark.md).
  The old name still works and warns; update your scripts.
- [`create_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/create_bookmark.md)
  and
  [`delete_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/delete_bookmark.md)
  no longer take `username`. Bookmarks always belong to the account that
  signed in, so the argument never did anything. Passing it warns and is
  ignored.
- [`get_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_member.md)
  returns the same 24 user columns as
  [`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
  after a `list_id` column. `is_verified` is now `verified` and
  `tweet_count` is now `post_count`.
- [`get_list_by_id()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_by_id.md)
  names its columns `list_id` and `list_name` (they were `id` and
  `name`). It,
  [`get_owned_list()`](https://Ivey-Business-School.github.io/xapir/reference/get_owned_list.md)
  and
  [`get_followed_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_followed_lists.md)
  now return the same eight columns: `list_id`, `list_name`,
  `description`, `created_at`, `follower_count`, `member_count`,
  `private` and `owner_id`.
- [`get_trends_by_woeid()`](https://Ivey-Business-School.github.io/xapir/reference/get_trends_by_woeid.md)
  names its count `post_count` (it was `tweet_count`).
- [`get_recent_post()`](https://Ivey-Business-School.github.io/xapir/reference/get_recent_post.md)
  reads up to 500 posts by default. It was 3,200, about \$16 a call.
  Pass `max_posts` for more.
- `max_posts` and `max_users` must be finite. `Inf` stops before the
  first request instead of reading until the budget runs out.
- Readers that used to return `NULL` or a bare
  [`tibble()`](https://tibble.tidyverse.org/reference/tibble.html) when
  there was nothing
  ([`get_owned_list()`](https://Ivey-Business-School.github.io/xapir/reference/get_owned_list.md),
  [`get_followed_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_followed_lists.md),
  [`get_list_by_id()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_by_id.md),
  [`get_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_member.md),
  [`get_trends_by_woeid()`](https://Ivey-Business-School.github.io/xapir/reference/get_trends_by_woeid.md),
  [`get_recent_post_count()`](https://Ivey-Business-School.github.io/xapir/reference/get_recent_post_count.md)
  and
  [`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md))
  now return a zero-row tibble with the usual columns. Test
  `nrow(x) == 0`, not `is.null(x)`.
- The package needs httr2 1.1.0 or newer. It already used a feature that
  arrived in 1.1.0, but asked only for 1.0.0, and on 1.0.0 every reader
  failed.

### New functions

The package was audited against the X API OpenAPI spec and the pricing
page, and 35 functions were added, for 81 in all. Grouped:

- Followers and following:
  [`get_followers()`](https://Ivey-Business-School.github.io/xapir/reference/get_followers.md),
  [`get_following()`](https://Ivey-Business-School.github.io/xapir/reference/get_following.md)
  and
  [`get_reposted_by()`](https://Ivey-Business-School.github.io/xapir/reference/get_reposted_by.md).
  Each returns the 24 user columns and stops at `max_users`.
- Lists:
  [`get_list_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_posts.md)
  (pages for the `extract_*()` tables),
  [`get_list_followers()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_followers.md),
  [`get_list_memberships()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_memberships.md),
  [`get_pinned_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_pinned_lists.md),
  [`create_list()`](https://Ivey-Business-School.github.io/xapir/reference/create_list.md),
  [`update_list()`](https://Ivey-Business-School.github.io/xapir/reference/update_list.md),
  [`delete_list()`](https://Ivey-Business-School.github.io/xapir/reference/delete_list.md),
  [`add_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/add_list_member.md),
  [`remove_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/remove_list_member.md),
  [`follow_list()`](https://Ivey-Business-School.github.io/xapir/reference/follow_list.md),
  [`unfollow_list()`](https://Ivey-Business-School.github.io/xapir/reference/unfollow_list.md),
  [`pin_list()`](https://Ivey-Business-School.github.io/xapir/reference/pin_list.md)
  and
  [`unpin_list()`](https://Ivey-Business-School.github.io/xapir/reference/unpin_list.md).
- Search the archive:
  [`get_all_post()`](https://Ivey-Business-School.github.io/xapir/reference/get_all_post.md)
  and
  [`get_all_post_count()`](https://Ivey-Business-School.github.io/xapir/reference/get_all_post_count.md),
  every public post back to 2006. Both need pay-per-use or Enterprise
  access; on a tier without it the call stops with the API’s own
  message.
- Your own account:
  [`get_usage()`](https://Ivey-Business-School.github.io/xapir/reference/get_usage.md)
  (posts read against the monthly cap),
  [`get_usage_credits()`](https://Ivey-Business-School.github.io/xapir/reference/get_usage_credits.md)
  (the dollar balance left),
  [`get_post_analytics()`](https://Ivey-Business-School.github.io/xapir/reference/get_post_analytics.md)
  (impressions, engagements and clicks on your posts, one row per post
  and period),
  [`get_personalized_trends()`](https://Ivey-Business-School.github.io/xapir/reference/get_personalized_trends.md)
  and
  [`search_users()`](https://Ivey-Business-School.github.io/xapir/reference/search_users.md).
- [`get_spend()`](https://Ivey-Business-School.github.io/xapir/reference/get_spend.md):
  the daily post reads from
  [`get_usage()`](https://Ivey-Business-School.github.io/xapir/reference/get_usage.md)
  priced at the post price, one row a day with `date`, `posts` and
  `dollars`, and one summary line. It counts post reads only and is a
  ceiling; the balance itself is
  [`get_usage_credits()`](https://Ivey-Business-School.github.io/xapir/reference/get_usage_credits.md).
- Interactions:
  [`like_post()`](https://Ivey-Business-School.github.io/xapir/reference/like_post.md),
  [`unlike_post()`](https://Ivey-Business-School.github.io/xapir/reference/unlike_post.md),
  [`block_user()`](https://Ivey-Business-School.github.io/xapir/reference/block_user.md)
  and
  [`unblock_user()`](https://Ivey-Business-School.github.io/xapir/reference/unblock_user.md).
- Media:
  [`upload_media()`](https://Ivey-Business-School.github.io/xapir/reference/upload_media.md)
  uploads a photo, GIF or video in chunks and returns the media id.
  [`create_post()`](https://Ivey-Business-School.github.io/xapir/reference/create_post.md)
  takes it as `media_ids`, and gained `quote_post_id`,
  `reply_to_post_id`, `poll_options`, `poll_duration_minutes`,
  `community_id`, `paid_partnership` and `share_with_followers` as plain
  arguments, so nobody builds nested lists.
- Spaces, communities and news:
  [`get_spaces()`](https://Ivey-Business-School.github.io/xapir/reference/get_spaces.md),
  [`search_spaces()`](https://Ivey-Business-School.github.io/xapir/reference/search_spaces.md),
  [`get_space_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_space_posts.md),
  [`get_community()`](https://Ivey-Business-School.github.io/xapir/reference/get_community.md),
  [`search_communities()`](https://Ivey-Business-School.github.io/xapir/reference/search_communities.md),
  [`search_news()`](https://Ivey-Business-School.github.io/xapir/reference/search_news.md)
  and
  [`get_news()`](https://Ivey-Business-School.github.io/xapir/reference/get_news.md).

Deliberately not covered: streaming, webhooks, the Activity API, direct
messages, Chat, Broadcasts, Community Notes, Compliance, Bots and
Articles.

### Prices

- Counts and trends are not free. The pricing page bills them per
  request: \$0.005 for a recent count, \$0.010 for an archive count,
  \$0.010 for trends.
  [`get_recent_post_count()`](https://Ivey-Business-School.github.io/xapir/reference/get_recent_post_count.md),
  [`get_all_post_count()`](https://Ivey-Business-School.github.io/xapir/reference/get_all_post_count.md),
  [`get_trends_by_woeid()`](https://Ivey-Business-School.github.io/xapir/reference/get_trends_by_woeid.md)
  and
  [`get_personalized_trends()`](https://Ivey-Business-School.github.io/xapir/reference/get_personalized_trends.md)
  say so before the request. Fields and expansions are still free.
- Your own data is billed at \$0.001 an item when the signed-in account
  owns the app (the pricing page’s “owned reads”). Readers of the
  signed-in account
  ([`get_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/get_bookmark.md),
  [`get_blocking()`](https://Ivey-Business-School.github.io/xapir/reference/get_blocking.md),
  [`get_muting()`](https://Ivey-Business-School.github.io/xapir/reference/get_muting.md),
  [`get_pinned_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_pinned_lists.md))
  price that way always; readers that take a `user_id`
  ([`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md),
  [`get_mentions()`](https://Ivey-Business-School.github.io/xapir/reference/get_mentions.md),
  [`get_liked_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_liked_posts.md),
  [`get_followers()`](https://Ivey-Business-School.github.io/xapir/reference/get_followers.md),
  [`get_following()`](https://Ivey-Business-School.github.io/xapir/reference/get_following.md),
  [`get_owned_list()`](https://Ivey-Business-School.github.io/xapir/reference/get_owned_list.md),
  [`get_followed_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_followed_lists.md),
  [`get_list_memberships()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_memberships.md))
  price that way when the id is yours. The package learns your id at
  sign-in, or from `options(xapir.my_user_id = "<id>")`. Checked against
  the credit balance on 24 September 2026: ten of your own posts cost
  \$0.01.
- Likes, mutes and blocks are billed per item at \$0.001:
  [`get_liking_users()`](https://Ivey-Business-School.github.io/xapir/reference/get_liking_users.md),
  [`get_muting()`](https://Ivey-Business-School.github.io/xapir/reference/get_muting.md)
  and
  [`get_blocking()`](https://Ivey-Business-School.github.io/xapir/reference/get_blocking.md)
  use that price. Followers, following and reposters are billed per user
  at \$0.010; lists, spaces and communities per item at \$0.005.
- Every price lives in one table, and
  `options(xapir.prices = list(posts = 0.006))` in `.Rprofile` overrides
  one entry. The older `xapir.price_per_post` and `xapir.price_per_user`
  options still work.
- Every write prints its price before the request:
  `This request costs about $0.015.` A post is \$0.015; a like, follow,
  repost, block or mute \$0.015 and its undo \$0.010; a list \$0.010 to
  create and \$0.005 to change; a bookmark, a deleted post or a hidden
  reply \$0.005.
- A post whose text carries a URL costs \$0.200, not \$0.015. The price
  line shows which rate applies before anything is sent.
- The pricing page also says reads are de-duplicated within a UTC day
  (reading the same post twice on the same day is billed once) and that
  pay-per-use is capped at 3 million post reads a month. Neither changes
  what a reader prints: the cap it announces is still the worst case.

### Signing in

- You sign in once. The user token is cached on disk under
  [`httr2::oauth_cache_path()`](https://httr2.r-lib.org/reference/oauth_cache_path.html)
  in a folder named `xapir`, reused in later R sessions, and refreshed
  in the background. To sign in as a different account, delete that
  folder and call a user function again.
- The sign-in asks for every permission the package needs.
  [`get_liked_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_liked_posts.md),
  [`get_muting()`](https://Ivey-Business-School.github.io/xapir/reference/get_muting.md),
  [`mute_user()`](https://Ivey-Business-School.github.io/xapir/reference/mute_user.md),
  [`unmute_user()`](https://Ivey-Business-School.github.io/xapir/reference/unmute_user.md)
  and
  [`hide_reply()`](https://Ivey-Business-School.github.io/xapir/reference/hide_reply.md)
  could not work with the token the package used to obtain, because the
  like, mute and moderate scopes were missing. If you signed in with an
  earlier version, sign in again once to pick the new scopes up.
- Sign-in and token refresh go to x.com, not twitter.com.

### What you will notice in your tables

- Every `extract_post_*()` table takes `include_referenced_posts`, like
  [`extract_post()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post.md)
  does. The default `TRUE` means a post that your timeline quoted,
  replied to or reposted gets its own rows in the media, url, mention,
  hashtag, cashtag, context, entity annotation, poll, place and edit
  tables too. Pass `FALSE` to keep only the posts the endpoint returned.
- Every table always has the same columns, even with no rows. A pull
  without polls gives a zero-row poll table, not `NULL`, so a script
  written on one pull runs on the next.
- [`extract_post_media()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_media.md)
  picks the `video/mp4` variant with the highest bit rate for a video or
  an animated GIF, and reports it in `bit_rate`.
- [`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md)
  and every user reader return 24 columns. New: `media_count`,
  `verified_followers_count`, `subscription_type` (`"Basic"`,
  `"Premium"`, `"PremiumPlus"` or `"None"`), `parody`,
  `profile_banner_url` and `pinned_post_id`.
- [`extract_post()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post.md)
  returns 24 columns. New: `paid_partnership` (after
  `possibly_sensitive`; `TRUE` when the author disclosed the post as
  paid promotion) and `community_id` (before `conversation_id`; `NA`
  unless the post was made in an X community).

### Reading from the API

- [`get_liking_users()`](https://Ivey-Business-School.github.io/xapir/reference/get_liking_users.md)
  returns the 24-column users table, like every other user reader. It
  returned raw pages before.

- [`get_mentions()`](https://Ivey-Business-School.github.io/xapir/reference/get_mentions.md)
  and
  [`get_liked_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_liked_posts.md)
  take `user_id` as an alternative to `username`, like
  [`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md).

- [`get_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/get_bookmark.md)
  no longer takes `username`. Bookmarks always belong to the account
  that signed in; passing a name warns and is ignored.

- [`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md),
  [`get_mentions()`](https://Ivey-Business-School.github.io/xapir/reference/get_mentions.md)
  and
  [`get_liked_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_liked_posts.md)
  accept `max_results` down to 5, the endpoint’s floor, so five posts
  cost five.

- [`get_post_analytics()`](https://Ivey-Business-School.github.io/xapir/reference/get_post_analytics.md)
  explains a 403 in plain words. The API’s own message says the app must
  be attached to a Project when it already is; the endpoint is simply
  closed to some accounts.

- A bare `Date` means local midnight everywhere:
  [`get_post_analytics()`](https://Ivey-Business-School.github.io/xapir/reference/get_post_analytics.md)
  used UTC midnight while
  [`iso_8601()`](https://Ivey-Business-School.github.io/xapir/reference/iso_8601.md)
  used local time, so a daily count could lose its first day.

- [`search_news()`](https://Ivey-Business-School.github.io/xapir/reference/search_news.md)
  returns stories in the language X chose for the app or account; the
  endpoint has no language parameter.

- [`get_trends_by_woeid()`](https://Ivey-Business-School.github.io/xapir/reference/get_trends_by_woeid.md)
  and
  [`get_personalized_trends()`](https://Ivey-Business-School.github.io/xapir/reference/get_personalized_trends.md)
  return `post_count` as `NA` when the API sends no number, which it
  often does.

- A new article, “The course workflow”
  ([`vignette("course-workflow")`](https://Ivey-Business-School.github.io/xapir/articles/course-workflow.md)),
  walks through the weekly routine for one brand: save the id, pull with
  `since_id`, save the pages, unfold, stack week over week, join, and
  check the bill with
  [`get_spend()`](https://Ivey-Business-School.github.io/xapir/reference/get_spend.md)
  and
  [`get_usage_credits()`](https://Ivey-Business-School.github.io/xapir/reference/get_usage_credits.md).

- Every reader prints the most it can spend before its first request,
  single-page readers included:
  `Reading up to 500 posts, about $2.50. Set max_posts to change this.`
  A paged reader prints what it actually read at the end:
  `Read 143 posts, about $0.72.`

- User reads are priced too, at \$0.010 per user:
  `Reading up to 2 users, about $0.02.`

- The prices are options; see Prices above.
  `options(xapir.prices = list(posts = 0.006))` moves one, and every
  message follows.

- `sleep_time` defaults to `0` everywhere.
  [`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md),
  [`get_account_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_account_timeline.md),
  [`get_liked_posts()`](https://Ivey-Business-School.github.io/xapir/reference/get_liked_posts.md)
  and
  [`get_liking_users()`](https://Ivey-Business-School.github.io/xapir/reference/get_liking_users.md)
  used to wait 90 seconds between pages; a rate limit already waits as
  long as X asks, so the pause bought nothing.

- [`get_blocking()`](https://Ivey-Business-School.github.io/xapir/reference/get_blocking.md),
  [`get_muting()`](https://Ivey-Business-School.github.io/xapir/reference/get_muting.md)
  and
  [`get_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_member.md)
  page through all their results, stop at `max_users` (default 500), and
  announce the cost first.

- [`get_owned_list()`](https://Ivey-Business-School.github.io/xapir/reference/get_owned_list.md)
  and
  [`get_followed_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_followed_lists.md)
  take `user_id` as an alternative to `username`, which skips the user
  read for the handle.

- The user and list readers
  ([`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md),
  [`get_users_by_usernames()`](https://Ivey-Business-School.github.io/xapir/reference/get_users_by_usernames.md),
  [`get_users_by_ids()`](https://Ivey-Business-School.github.io/xapir/reference/get_users_by_ids.md),
  [`get_my_user()`](https://Ivey-Business-School.github.io/xapir/reference/get_my_user.md),
  [`get_blocking()`](https://Ivey-Business-School.github.io/xapir/reference/get_blocking.md),
  [`get_muting()`](https://Ivey-Business-School.github.io/xapir/reference/get_muting.md),
  [`get_list_member()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_member.md),
  [`get_owned_list()`](https://Ivey-Business-School.github.io/xapir/reference/get_owned_list.md),
  [`get_followed_lists()`](https://Ivey-Business-School.github.io/xapir/reference/get_followed_lists.md),
  [`get_list_by_id()`](https://Ivey-Business-School.github.io/xapir/reference/get_list_by_id.md)
  and
  [`get_trends_by_woeid()`](https://Ivey-Business-School.github.io/xapir/reference/get_trends_by_woeid.md))
  share one 24-column user schema, read only the API’s `data` block, and
  warn when the API reports a partial error (a suspended account among
  the ids, say) instead of building rows out of the error.

- [`get_recent_post_count()`](https://Ivey-Business-School.github.io/xapir/reference/get_recent_post_count.md)
  returns a zero-row tibble with `start`, `end` and `post_count` when
  the query matched nothing.

### Writing to X

- Every write function goes through the same request layer as the
  readers: a rate limit (429) or a server error (5xx) is retried,
  waiting as long as X asks; anything else stops with the API’s own
  message. Each returns the API’s `data` invisibly.
  [`unfollow_user()`](https://Ivey-Business-School.github.io/xapir/reference/unfollow_user.md)
  used to swallow a failure and the rest threw the raw response.
- [`delete_post()`](https://Ivey-Business-School.github.io/xapir/reference/delete_post.md)
  returns exactly one row per id, with `post_id`, `deleted` and `error`.
  A post that could not be deleted keeps its row and its message instead
  of disappearing.
- [`create_post()`](https://Ivey-Business-School.github.io/xapir/reference/create_post.md)
  no longer refuses text over 280 characters. Premium accounts may post
  longer text; the API says no when it is too long.
- [`delete_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/delete_bookmark.md)
  sent its request to the wrong address (the bookmarks collection,
  without the post id) and could not remove anything. Fixed.

### Under the hood

- A Getting Started vignette replaces the empty draft: signing in, what
  a call costs, read-save-unfold, the twelve tables, and writing to X.
  Nothing in it calls the API when it is built.
- R CMD check and test coverage run on every push. The stale committed
  `docs/` folder is gone; the site is built by the pkgdown workflow.
- tidyr is no longer a dependency; nothing in the package used it.
- The package imports curl, for the multipart requests that
  [`upload_media()`](https://Ivey-Business-School.github.io/xapir/reference/upload_media.md)
  sends.
- 81 exported functions, after an audit of the package against the X API
  OpenAPI spec (version 2.168) and the pricing page of 24
  September 2026. Every endpoint the package covers was checked for its
  arguments, its page size and its price.
- Tests cover the write functions, the user and list readers, the media
  upload, and the cost lines and caps of every reader: 1,427
  expectations. Nothing in the tests calls the API.

## xapir 0.1.1

Install it with `pak::pak("Ivey-Business-School/xapir@v0.1.1")`.

### Reading from the API

- [`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md)
  takes `user_id` as an alternative to `username`. A handle costs one
  user read (\$0.010) to turn it into an id before the posts are read.
  When you already know the id, pass `user_id` and that read is skipped;
  the posts cost the same as before. Give one of the two, not both, or
  the function stops before it spends anything. Keep the id as text:
  `user_id = "2244994945"`.

## xapir 0.1.0

The first numbered release. Install it with
`pak::pak("Ivey-Business-School/xapir@v0.1.0")`.

### What you will notice in your post table

- Long posts are no longer cut off. X returns posts over 280 characters
  with a short `text` and the full text in `note_tweet`.
  [`extract_post()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post.md)
  now puts the full text in `text` and marks the row
  `is_long_post = TRUE`. There is one text column, and it is the whole
  post.
- One row per post. A post that appeared in one page’s `data` and
  another page’s `includes` used to come back twice, and a post that
  both replied and quoted came back twice too. Both are fixed. Thread
  labels are cleaner as a result.
- `post_url` carries the author’s handle:
  `https://x.com/<username>/status/<post_id>`. When the author is not in
  the response, it falls back to `https://x.com/i/web/status/<post_id>`,
  which X also resolves.
- Reposts keep their own `impression_count`. Their `like_count`,
  `reply_count`, `quote_count`, `bookmark_count` and `repost_count` are
  `NA`, because those numbers belong to the original post.
- New columns: `lang`, `possibly_sensitive`, and `article_title` when
  the timeline holds an X article.
- `created_at` is in UTC by default, the same as
  [`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md).
  Convert with
  [`lubridate::with_tz()`](https://lubridate.tidyverse.org/reference/with_tz.html)
  when you want local time, or pass `tz = Sys.timezone()`.

### Other tables

- [`extract_user()`](https://Ivey-Business-School.github.io/xapir/reference/extract_user.md)
  adds `is_identity_verified` and `url` (the profile link as the API
  sends it; `link_in_bio` is still its display form).
- [`extract_post_media()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_media.md)
  adds `alt_text`.
- [`extract_post_edited_post_id()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_edited_post_id.md)
  no longer lists every post as an edit of itself. A row now means the
  post was edited.
- [`extract_post_poll_option()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post_poll_option.md)
  finds polls on quoted posts too, and never returns a row with an `NA`
  `post_id`.

### Reading from the API

- Every posts reader asks for the same fields by default, including
  `note_tweet`, `article`, `edit_controls`, `possibly_sensitive`,
  `is_identity_verified`, `url`, `alt_text` and the `geo.place_id`
  expansion. Fields are free; posts cost.
- [`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md)
  reads up to 500 posts by default (it was 3,200, about \$16) and prints
  the cap in posts and dollars before its first request. Pass
  `max_posts` to change it.
- No more endless retries. A rate limit (429) or a server error (5xx) is
  retried up to three times, waiting as long as X asks. Anything else, a
  bad token or a misspelled handle, stops in seconds with X’s own
  message.
- `max_results` must be between 10 and 100, and the function tells you
  so before it spends anything.
- [`get_recent_post()`](https://Ivey-Business-School.github.io/xapir/reference/get_recent_post.md)
  never returns more posts than `max_posts`.
- [`get_mentions()`](https://Ivey-Business-School.github.io/xapir/reference/get_mentions.md)
  is exported: the posts that name an account, with `since_id` for
  appending only what is new.
  [`get_account_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_account_timeline.md)
  and
  [`get_bookmark()`](https://Ivey-Business-School.github.io/xapir/reference/get_bookmark.md)
  are exported under their own names too; all three used to be hidden
  behind a duplicate
  [`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md)
  definition.

### Under the hood

- [`library(xapir)`](https://github.com/Ivey-Business-School/xapir)
  works on its own. Every function the package uses is imported, so you
  no longer need the tidyverse attached for
  [`extract_post()`](https://Ivey-Business-School.github.io/xapir/reference/extract_post.md)
  to run.
- The package needs R 4.1 or newer (it uses the native pipe).
- Tests run on a saved two-page Tesla timeline. Nothing in the tests
  calls the API.

## xapir 0.0.0.9000

- Development versions used in the 2025 course. No release notes were
  kept.
