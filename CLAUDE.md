# xapir

An R client for the X API v2, written for students in a marketing course.
Readable code and clear messages matter more than cleverness. X bills per
item read and per write, so every function that spends prints what it can
cost before it spends.

## Layout

- `R/utils-request.R`: the request layer every function uses. `x_request()`
  builds a request on `https://api.x.com/2`; `x_perform()` retries a 429 or
  5xx as long as X asks and stops on any other error with the API's own
  message; `fetch_pages()` walks a paginated endpoint up to a cap. The price
  table (`x_default_prices`, `x_price()`), the cost lines (`announce_cap()`,
  `announce_total()`, `announce_request_cost()`) and the input checks
  (`check_max_results()`, `check_max_posts()`, `check_post_ids()`,
  `check_one_of_user()`) live here. The default field sets do too.
- `R/utils-auth.R`: the OAuth scope list, `my_user_id()`, and the id checks
  the write functions share. `R/authenticate_user.R` gets the user token
  through `httr2::oauth_token_cached()`; it caches on disk under
  `httr2::oauth_cache_path()/xapir`.
- `R/utils-user.R`: `user_row()`, `user_schema()` and `users_table()`, the
  one 24-column user table every user reader returns; the same for lists
  and trends.
- `R/utils-post-list.R`: how a saved response (a list of pages) is read.
  `post_list()` and `unique_posts()` return the posts in `data` and
  `includes$tweets`, data copy first.
- `R/get_*.R`, `R/search_*.R`: readers. Posts readers return a list of
  pages the `extract_*()` functions accept. User, list, space, community
  and news readers return typed tibbles.
- `R/extract_*.R`: the twelve tables of the course data model, built from a
  saved response. None of them calls the API.
- `R/create_*.R`, `R/delete_*.R`, `like_post.R`, `follow_user.R` and the
  rest of the writes: user-token functions that return `invisible(response$data)`.
- `man-roxygen/`: shared `@param` text, pulled in with `@template`.

## Rules every function follows

1. Validate every argument before the first request. Ids are strings of
   digits; a numeric id stops with "keep ids as text".
2. Print the cost before spending. Readers call `announce_cap()` with the
   right `what` (`posts`, `users`, `follows`, `likes`, `lists`, ...); writes
   and counts call `announce_request_cost()`. Prices come from the pricing
   page, dated in the comment above `x_default_prices`. Your own data bills
   as `owned` when the package knows the account is yours (`is_owned()`).
3. Return a typed zero-row tibble when nothing comes back, never `NULL` or
   a bare `tibble()`. Warn once, with the API's detail, on a partial
   failure (`warn_partial_errors()`).
4. Say "post", not "tweet", in every argument name and every sentence. The
   API still uses `tweet.fields`, `note_tweet` and `referenced_tweets` on
   the wire; keep those as the API sends them.
5. Roxygen: title, one paragraph with the docs.x.com link, "Needs a user
   token, so the first call opens a browser window to sign in." on
   user-token functions, `@param` for everything, `@return` listing the
   columns, examples in `\dontrun{}`. Add an `@importFrom` for every
   non-base function the file calls; `NAMESPACE` and `man/` are generated.

## Working on it

```r
devtools::load_all()
testthat::test_dir("tests/testthat")   # 1,400+ expectations, none call the API
roxygen2::roxygenise()                  # after any roxygen change
spelling::spell_check_package(vignettes = TRUE)
rmarkdown::render("README.Rmd"); rmarkdown::render("index.Rmd")
```

- Tests mock every request with `httr2::local_mocked_responses()`. The
  shared helpers are in `tests/testthat/helper-fixtures.R`: `json_response()`,
  `posts_page()`, `users_page()`, `fake_token`, `mock_user_token()`,
  `record_requests()`, `sent_json()`, `collect_messages()`, `auth_header()`.
  Read a bearer header with `auth_header(req)`, never `req$headers$Authorization`:
  httr2 1.2.0 stores it redacted.
- `tests/testthat/fixtures/tesla-2-pages.rds` is a real two-page timeline
  from September 2026 and the source of truth for response shapes.
- Format dollars with `dollars()`. `sprintf("%.2f")` rounds a half cent
  differently on Windows.
- CI runs R CMD check on five platforms, coverage and pkgdown. Push to
  `master` only when the suite passes here; if you can, run it under the
  newest httr2 as well as the oldest DESCRIPTION allows.
- `_pkgdown.yml` must list every export exactly once or the site build
  fails. Compare against `NAMESPACE` after adding a function.
- Version and NEWS move together. The install line in NEWS names a tag;
  push the tag when the release is cut.

## What the API does that surprises people

- The OpenAPI spec (docs.x.com, `xdevplatform/docs` on GitHub) names
  parameters `post.fields`; the live API accepts and returns the `tweet.*`
  names. Do not rename.
- Counts and trends are billed per request, not free. A post whose text
  carries a URL costs $0.200 to create, not $0.015.
- `GET /2/tweets/analytics` returns 403 for some pay-per-use accounts with
  a message about a missing Project the app already has.
- `edit_history_tweet_ids` on a freshly created post comes back as `""`.
- Trends often arrive with no `post_count`; the column is `NA` then.
- The OAuth callback must be exactly `http://localhost:1410`, and the
  sign-in needs an interactive session (`options(rlang_interactive = TRUE)`
  under `Rscript`).

## Out of scope

Streaming, webhooks, the Activity API, direct messages, Chat, Broadcasts,
Community Notes, Compliance, Bots and Articles. None of them fit a course
package about public engagement, and the streams need a long-lived
connection.
