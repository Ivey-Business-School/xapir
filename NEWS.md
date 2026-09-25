# xapir 0.2.1

Install it with `pak::pak("Ivey-Business-School/xapir@v0.2.1")`.

Three fixes from a second read-only run against the live API on
24 September 2026, after the 0.2.0 tag was cut.

* Three user fields the spec lists, `verified_followers_count`,
  `subscription_type` and `parody`, are refused to an app token, so they
  are no longer requested by default. Their columns stay in the user
  table as `NA`; ask for them with `user_fields` when your token can read
  them. A refused field now warns once, naming the fields, instead of
  being counted as a user that could not be read.
* `get_my_user()` remembers whose account signed in, so a later read of
  that account's own posts or followers by `user_id` is priced as owned.
* `get_owned_list()` and `get_followed_lists()` print what they read
  after the page comes back, like the paged readers.

# xapir 0.2.0

Install it with `pak::pak("Ivey-Business-School/xapir@v0.2.0")`.

## Breaking changes

* `tweet_id` is now `post_id` in `create_repost()`, `delete_repost()`,
  `create_bookmark()` and `delete_bookmark()`. The old name still works
  and warns; update your scripts.
* `create_bookmark()` and `delete_bookmark()` no longer take `username`.
  Bookmarks always belong to the account that signed in, so the argument
  never did anything. Passing it warns and is ignored.
* `get_list_member()` returns the same 24 user columns as
  `extract_user()`, after a `list_id` column. `is_verified` is now
  `verified` and `tweet_count` is now `post_count`.
* `get_list_by_id()` names its columns `list_id` and `list_name` (they
  were `id` and `name`). It, `get_owned_list()` and
  `get_followed_lists()` now return the same eight columns: `list_id`,
  `list_name`, `description`, `created_at`, `follower_count`,
  `member_count`, `private` and `owner_id`.
* `get_trends_by_woeid()` names its count `post_count` (it was
  `tweet_count`).
* `get_recent_post()` reads up to 500 posts by default. It was 3,200,
  about $16 a call. Pass `max_posts` for more.
* `max_posts` and `max_users` must be finite. `Inf` stops before the
  first request instead of reading until the budget runs out.
* Readers that used to return `NULL` or a bare `tibble()` when there was
  nothing (`get_owned_list()`, `get_followed_lists()`, `get_list_by_id()`,
  `get_list_member()`, `get_trends_by_woeid()`, `get_recent_post_count()`
  and `extract_user()`) now return a zero-row tibble with the usual
  columns. Test `nrow(x) == 0`, not `is.null(x)`.
* The package needs httr2 1.1.0 or newer. It already used a feature that
  arrived in 1.1.0, but asked only for 1.0.0, and on 1.0.0 every reader
  failed.

## New functions

The package was audited against the X API OpenAPI spec and the pricing
page, and 35 functions were added, for 81 in all. Grouped:

* Followers and following: `get_followers()`, `get_following()` and
  `get_reposted_by()`. Each returns the 24 user columns and stops at
  `max_users`.
* Lists: `get_list_posts()` (pages for the `extract_*()` tables),
  `get_list_followers()`, `get_list_memberships()`, `get_pinned_lists()`,
  `create_list()`, `update_list()`, `delete_list()`, `add_list_member()`,
  `remove_list_member()`, `follow_list()`, `unfollow_list()`, `pin_list()`
  and `unpin_list()`.
* Search the archive: `get_all_post()` and `get_all_post_count()`, every
  public post back to 2006. Both need pay-per-use or Enterprise access; on
  a tier without it the call stops with the API's own message.
* Your own account: `get_usage()` (posts read against the monthly cap),
  `get_usage_credits()` (the dollar balance left), `get_post_analytics()`
  (impressions, engagements and clicks on your posts, one row per post
  and period), `get_personalized_trends()` and `search_users()`.
* `get_spend()`: the daily post reads from `get_usage()` priced at the
  post price, one row a day with `date`, `posts` and `dollars`, and one
  summary line. It counts post reads only and is a ceiling; the balance
  itself is `get_usage_credits()`.
* Interactions: `like_post()`, `unlike_post()`, `block_user()` and
  `unblock_user()`.
* Media: `upload_media()` uploads a photo, GIF or video in chunks and
  returns the media id. `create_post()` takes it as `media_ids`, and
  gained `quote_post_id`, `reply_to_post_id`, `poll_options`,
  `poll_duration_minutes`, `community_id`, `paid_partnership` and
  `share_with_followers` as plain arguments, so nobody builds nested
  lists.
* Spaces, communities and news: `get_spaces()`, `search_spaces()`,
  `get_space_posts()`, `get_community()`, `search_communities()`,
  `search_news()` and `get_news()`.

Deliberately not covered: streaming, webhooks, the Activity API, direct
messages, Chat, Broadcasts, Community Notes, Compliance, Bots and
Articles.

## Prices

* Counts and trends are not free. The pricing page bills them per
  request: $0.005 for a recent count, $0.010 for an archive count, $0.010
  for trends. `get_recent_post_count()`, `get_all_post_count()`,
  `get_trends_by_woeid()` and `get_personalized_trends()` say so before
  the request. Fields and expansions are still free.
* Your own data is billed at $0.001 an item when the signed-in account
  owns the app (the pricing page's "owned reads"). Readers of the
  signed-in account (`get_bookmark()`, `get_blocking()`, `get_muting()`,
  `get_pinned_lists()`) price that way always; readers that take a
  `user_id` (`get_timeline()`, `get_mentions()`, `get_liked_posts()`,
  `get_followers()`, `get_following()`, `get_owned_list()`,
  `get_followed_lists()`, `get_list_memberships()`) price that way when
  the id is yours. The package learns your id at sign-in, or from
  `options(xapir.my_user_id = "<id>")`. Checked against the credit
  balance on 24 September 2026: ten of your own posts cost $0.01.
* Likes, mutes and blocks are billed per item at $0.001: `get_liking_users()`,
  `get_muting()` and `get_blocking()` use that price. Followers,
  following and reposters are billed per user at $0.010; lists, spaces and
  communities per item at $0.005.
* Every price lives in one table, and `options(xapir.prices = list(posts
  = 0.006))` in `.Rprofile` overrides one entry. The older
  `xapir.price_per_post` and `xapir.price_per_user` options still work.
* Every write prints its price before the request: `This request costs
  about $0.015.` A post is $0.015; a like, follow, repost, block or mute
  $0.015 and its undo $0.010; a list $0.010 to create and $0.005 to change;
  a bookmark, a deleted post or a hidden reply $0.005.
* A post whose text carries a URL costs $0.200, not $0.015. The price
  line shows which rate applies before anything is sent.
* The pricing page also says reads are de-duplicated within a UTC day
  (reading the same post twice on the same day is billed once) and that
  pay-per-use is capped at 3 million post reads a month. Neither changes
  what a reader prints: the cap it announces is still the worst case.

## Signing in

* You sign in once. The user token is cached on disk under
  `httr2::oauth_cache_path()` in a folder named `xapir`, reused in later
  R sessions, and refreshed in the background. To sign in as a different
  account, delete that folder and call a user function again.
* The sign-in asks for every permission the package needs.
  `get_liked_posts()`, `get_muting()`, `mute_user()`, `unmute_user()` and
  `hide_reply()` could not work with the token the package used to obtain,
  because the like, mute and moderate scopes were missing. If you signed
  in with an earlier version, sign in again once to pick the new scopes
  up.
* Sign-in and token refresh go to x.com, not twitter.com.

## What you will notice in your tables

* Every `extract_post_*()` table takes `include_referenced_posts`, like
  `extract_post()` does. The default `TRUE` means a post that your
  timeline quoted, replied to or reposted gets its own rows in the media,
  url, mention, hashtag, cashtag, context, entity annotation, poll, place
  and edit tables too. Pass `FALSE` to keep only the posts the endpoint
  returned.
* Every table always has the same columns, even with no rows. A pull
  without polls gives a zero-row poll table, not `NULL`, so a script
  written on one pull runs on the next.
* `extract_post_media()` picks the `video/mp4` variant with the highest
  bit rate for a video or an animated GIF, and reports it in `bit_rate`.
* `extract_user()` and every user reader return 24 columns. New:
  `media_count`, `verified_followers_count`, `subscription_type`
  (`"Basic"`, `"Premium"`, `"PremiumPlus"` or `"None"`), `parody`,
  `profile_banner_url` and `pinned_post_id`.
* `extract_post()` returns 24 columns. New: `paid_partnership` (after
  `possibly_sensitive`; `TRUE` when the author disclosed the post as paid
  promotion) and `community_id` (before `conversation_id`; `NA` unless the
  post was made in an X community).

## Reading from the API

* `get_liking_users()` returns the 24-column users table, like every
  other user reader. It returned raw pages before.
* `get_mentions()` and `get_liked_posts()` take `user_id` as an
  alternative to `username`, like `get_timeline()`.
* `get_bookmark()` no longer takes `username`. Bookmarks always belong to
  the account that signed in; passing a name warns and is ignored.
* `get_timeline()`, `get_mentions()` and `get_liked_posts()` accept
  `max_results` down to 5, the endpoint's floor, so five posts cost five.
* `get_post_analytics()` explains a 403 in plain words. The API's own
  message says the app must be attached to a Project when it already is;
  the endpoint is simply closed to some accounts.
* A bare `Date` means local midnight everywhere: `get_post_analytics()`
  used UTC midnight while `iso_8601()` used local time, so a daily count
  could lose its first day.
* `search_news()` returns stories in the language X chose for the app
  or account; the endpoint has no language parameter.
* `get_trends_by_woeid()` and `get_personalized_trends()` return
  `post_count` as `NA` when the API sends no number, which it often does.
* A new article, "The course workflow" (`vignette("course-workflow")`),
  walks through the weekly routine for one brand: save the id, pull with
  `since_id`, save the pages, unfold, stack week over week, join, and
  check the bill with `get_spend()` and `get_usage_credits()`.


* Every reader prints the most it can spend before its first request,
  single-page readers included: `Reading up to 500 posts, about $2.50.
  Set max_posts to change this.` A paged reader prints what it actually
  read at the end: `Read 143 posts, about $0.72.`
* User reads are priced too, at $0.010 per user: `Reading up to 2 users,
  about $0.02.`
* The prices are options; see Prices above. `options(xapir.prices =
  list(posts = 0.006))` moves one, and every message follows.
* `sleep_time` defaults to `0` everywhere. `get_timeline()`,
  `get_account_timeline()`, `get_liked_posts()` and `get_liking_users()`
  used to wait 90 seconds between pages; a rate limit already waits as
  long as X asks, so the pause bought nothing.
* `get_blocking()`, `get_muting()` and `get_list_member()` page through
  all their results, stop at `max_users` (default 500), and announce the
  cost first.
* `get_owned_list()` and `get_followed_lists()` take `user_id` as an
  alternative to `username`, which skips the user read for the handle.
* The user and list readers (`extract_user()`, `get_users_by_usernames()`,
  `get_users_by_ids()`, `get_my_user()`, `get_blocking()`, `get_muting()`,
  `get_list_member()`, `get_owned_list()`, `get_followed_lists()`,
  `get_list_by_id()` and `get_trends_by_woeid()`) share one 24-column
  user schema, read only the API's `data` block, and warn when the API
  reports a partial error (a suspended account among the ids, say)
  instead of building rows out of the error.
* `get_recent_post_count()` returns a zero-row tibble with `start`, `end`
  and `post_count` when the query matched nothing.

## Writing to X

* Every write function goes through the same request layer as the
  readers: a rate limit (429) or a server error (5xx) is retried, waiting
  as long as X asks; anything else stops with the API's own message. Each
  returns the API's `data` invisibly. `unfollow_user()` used to swallow a
  failure and the rest threw the raw response.
* `delete_post()` returns exactly one row per id, with `post_id`,
  `deleted` and `error`. A post that could not be deleted keeps its row
  and its message instead of disappearing.
* `create_post()` no longer refuses text over 280 characters. Premium
  accounts may post longer text; the API says no when it is too long.
* `delete_bookmark()` sent its request to the wrong address (the bookmarks
  collection, without the post id) and could not remove anything. Fixed.

## Under the hood

* A Getting Started vignette replaces the empty draft: signing in, what a
  call costs, read-save-unfold, the twelve tables, and writing to X.
  Nothing in it calls the API when it is built.
* R CMD check and test coverage run on every push. The stale committed
  `docs/` folder is gone; the site is built by the pkgdown workflow.
* tidyr is no longer a dependency; nothing in the package used it.
* The package imports curl, for the multipart requests that
  `upload_media()` sends.
* 81 exported functions, after an audit of the package against the X API
  OpenAPI spec (version 2.168) and the pricing page of 24 September 2026.
  Every endpoint the package covers was checked for its arguments, its
  page size and its price.
* Tests cover the write functions, the user and list readers, the media
  upload, and the cost lines and caps of every reader: 1,427 expectations.
  Nothing in the tests calls the API.

# xapir 0.1.1

Install it with `pak::pak("Ivey-Business-School/xapir@v0.1.1")`.

## Reading from the API

* `get_timeline()` takes `user_id` as an alternative to `username`. A
  handle costs one user read ($0.010) to turn it into an id before the
  posts are read. When you already know the id, pass `user_id` and that
  read is skipped; the posts cost the same as before. Give one of the
  two, not both, or the function stops before it spends anything. Keep
  the id as text: `user_id = "2244994945"`.

# xapir 0.1.0

The first numbered release. Install it with
`pak::pak("Ivey-Business-School/xapir@v0.1.0")`.

## What you will notice in your post table

* Long posts are no longer cut off. X returns posts over 280 characters
  with a short `text` and the full text in `note_tweet`. `extract_post()`
  now puts the full text in `text` and marks the row `is_long_post = TRUE`.
  There is one text column, and it is the whole post.
* One row per post. A post that appeared in one page's `data` and another
  page's `includes` used to come back twice, and a post that both replied
  and quoted came back twice too. Both are fixed. Thread labels are
  cleaner as a result.
* `post_url` carries the author's handle:
  `https://x.com/<username>/status/<post_id>`. When the author is not in
  the response, it falls back to `https://x.com/i/web/status/<post_id>`,
  which X also resolves.
* Reposts keep their own `impression_count`. Their `like_count`,
  `reply_count`, `quote_count`, `bookmark_count` and `repost_count` are
  `NA`, because those numbers belong to the original post.
* New columns: `lang`, `possibly_sensitive`, and `article_title` when the
  timeline holds an X article.
* `created_at` is in UTC by default, the same as `extract_user()`. Convert
  with `lubridate::with_tz()` when you want local time, or pass
  `tz = Sys.timezone()`.

## Other tables

* `extract_user()` adds `is_identity_verified` and `url` (the profile link
  as the API sends it; `link_in_bio` is still its display form).
* `extract_post_media()` adds `alt_text`.
* `extract_post_edited_post_id()` no longer lists every post as an edit of
  itself. A row now means the post was edited.
* `extract_post_poll_option()` finds polls on quoted posts too, and never
  returns a row with an `NA` `post_id`.

## Reading from the API

* Every posts reader asks for the same fields by default, including
  `note_tweet`, `article`, `edit_controls`, `possibly_sensitive`,
  `is_identity_verified`, `url`, `alt_text` and the `geo.place_id`
  expansion. Fields are free; posts cost.
* `get_timeline()` reads up to 500 posts by default (it was 3,200, about
  $16) and prints the cap in posts and dollars before its first request.
  Pass `max_posts` to change it.
* No more endless retries. A rate limit (429) or a server error (5xx) is
  retried up to three times, waiting as long as X asks. Anything else, a
  bad token or a misspelled handle, stops in seconds with X's own message.
* `max_results` must be between 10 and 100, and the function tells you so
  before it spends anything.
* `get_recent_post()` never returns more posts than `max_posts`.
* `get_mentions()` is exported: the posts that name an account, with
  `since_id` for appending only what is new. `get_account_timeline()` and
  `get_bookmark()` are exported under their own names too; all three used
  to be hidden behind a duplicate `get_timeline()` definition.

## Under the hood

* `library(xapir)` works on its own. Every function the package uses is
  imported, so you no longer need the tidyverse attached for
  `extract_post()` to run.
* The package needs R 4.1 or newer (it uses the native pipe).
* Tests run on a saved two-page Tesla timeline. Nothing in the tests calls
  the API.

# xapir 0.0.0.9000

* Development versions used in the 2025 course. No release notes were kept.
