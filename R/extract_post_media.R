#' Extract Media Information from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle media
#' information, such as images, videos, and GIFs attached to posts. Each row
#' is one media item attached to one post, so a post with three photos gives
#' three rows.
#'
#' `alt_text` is the author's description of the media and is `NA` when none
#' was written. For a video or animated GIF, `url` and `bit_rate` come from
#' the `video/mp4` variant with the highest bit rate, which is the best
#' quality the API offers; for a photo, `url` is the image and `bit_rate` is
#' `NA`.
#'
#' Each post appears once, even when it sits in one page's `data` and another
#' page's `includes$tweets`. The `data` copy wins.
#'
#' @importFrom purrr map keep
#' @importFrom dplyr bind_rows left_join select all_of distinct
#' @importFrom tibble tibble
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param include_referenced_posts Logical. Whether to include the posts in
#'   `includes$tweets` (the posts that were quoted, replied to or reposted).
#'   Defaults to TRUE.
#' @return A tibble with one row per media item per post and the columns
#'   `post_id`, `media_id`, `type`, `view_count`, `duration_ms`, `height`,
#'   `width`, `preview_image_url`, `url`, `alt_text` and `bit_rate`. Ids are
#'   character; `view_count`, `duration_ms`, `height`, `width` and `bit_rate`
#'   are integer. A timeline without media gives zero rows with the same
#'   columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_media <- extract_post_media(timeline)
#' }
#' @export
extract_post_media <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  media_schema <- tibble(
    post_id           = character(0),
    media_id          = character(0),
    type              = character(0),
    view_count        = integer(0),
    duration_ms       = integer(0),
    height            = integer(0),
    width             = integer(0),
    preview_image_url = character(0),
    url               = character(0),
    alt_text          = character(0),
    bit_rate          = integer(0)
  )

  # Step 1: which media keys does each post attach?
  posts <- unique_posts(timeline, include_referenced_posts)

  post_media_map <- bind_rows(
    select(media_schema, post_id, media_id),
    map(posts, function(x) {
      keys <- x$attachments$media_keys
      if (is.null(keys)) return(NULL)
      tibble(post_id = x$id, media_id = as.character(unlist(keys)))
    })
  )

  # Step 2: the details of every media item in includes$media, each once
  media_tbl <- bind_rows(
    select(media_schema, -post_id),
    map(unique_includes(timeline, "media", id_field = "media_key"), media_row)
  )

  # Step 3: attach the details to each post's media keys
  post_media_map |>
    left_join(media_tbl, by = "media_id") |>
    select(all_of(names(media_schema))) |>
    distinct()
}

#' One row of media details from one item of includes$media
#' @keywords internal
#' @noRd
media_row <- function(x) {
  best <- best_mp4(x$variants %||% list())

  tibble(
    media_id          = x$media_key %||% NA_character_,
    type              = x$type %||% NA_character_,
    view_count        = x$public_metrics$view_count %||% NA_integer_,
    duration_ms       = x$duration_ms %||% NA_integer_,
    height            = x$height %||% NA_integer_,
    width             = x$width %||% NA_integer_,
    preview_image_url = x$preview_image_url %||% NA_character_,
    url               = best$url %||% x$url %||% NA_character_,
    alt_text          = x$alt_text %||% NA_character_,
    bit_rate          = best$bit_rate %||% NA_integer_
  )
}

#' The video/mp4 variant with the highest bit rate, or NULL when there is none
#' @keywords internal
#' @noRd
best_mp4 <- function(variants) {
  mp4 <- keep(variants, ~ identical(.x$content_type, "video/mp4"))
  if (length(mp4) == 0) return(NULL)
  bit_rates <- vapply(mp4, function(v) as.numeric(v$bit_rate %||% 0), numeric(1))
  mp4[[which.max(bit_rates)]]
}
