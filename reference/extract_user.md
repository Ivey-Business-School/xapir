# Extract User Data from Timeline

Reads the users that a timeline's pages carry in `includes$users` (the
authors of the posts, and the accounts they mention, reply to or quote)
and returns one row per user. `created_at` is in UTC.
`is_identity_verified` says whether X has checked the account holder's
identity document; `url` is the profile link as the API returns it (a
t.co address), and `link_in_bio` is its display form.

## Usage

``` r
extract_user(timeline)
```

## Arguments

- timeline:

  A list of pages as returned by a reader such as
  [`get_timeline()`](https://Ivey-Business-School.github.io/xapir/reference/get_timeline.md).

## Value

A tibble with one row per user id and 24 columns: `created_at` (POSIXct,
UTC), `username`, `name`, `description`, `followers_count`,
`following_count`, `post_count`, `listed_count`, `like_count`,
`media_count`, `protected`, `verified`, `verified_type`,
`verified_followers_count`, `subscription_type` (`"Basic"`, `"Premium"`,
`"PremiumPlus"` or `"None"`), `parody`, `is_identity_verified`,
`location`, `profile_image_url`, `profile_banner_url`, `link_in_bio`,
`url`, `pinned_post_id` and `user_id`. A field the API leaves out is
`NA`. A timeline with no users gives the same columns and no rows.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username    = "XDevelopers",
  max_results = 100,
  start_time  = iso_8601(Sys.Date() - 7)
)
user <- extract_user(timeline)
} # }
```
