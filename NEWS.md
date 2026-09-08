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
