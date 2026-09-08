# Extract Media Information from Timeline

Processes the timeline data retrieved from the X API to wrangle media
information, such as images, videos, and GIFs attached to posts.
`alt_text` is the author's description of the media and is `NA` when
none was written.

## Usage

``` r
extract_post_media(timeline)
```

## Arguments

- timeline:

  A list containing the timeline data retrieved from the X API.

## Value

A tibble containing structured media data.

## Examples

``` r
if (FALSE) { # \dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_media <- extract_post_media(timeline)
} # }
```
