# Extract User Data from Timeline

Processes the timeline data retrieved from the X API to extract user
metadata, including profile details and public metrics. `created_at` is
in UTC. `is_identity_verified` says whether X has checked the account
holder's identity document; `url` is the profile link as the API returns
it (a t.co address), and `link_in_bio` is its display form.

## Usage

``` r
extract_user(timeline)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

## Value

A tibble containing structured user data.

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
