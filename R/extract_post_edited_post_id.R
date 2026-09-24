#' Extract Post Edited Post ID from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to retrieve data on
#' previous versions of posts. The API lists a post's own id in its edit
#' history, so an unedited post lists only itself. Those self rows are
#' dropped: a row here means the post was edited, and `edited_post_id` is an
#' earlier version.
#'
#' Each post appears once, even when it sits in one page's `data` and another
#' page's `includes$tweets`. The `data` copy wins.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows filter distinct
#' @importFrom tibble tibble
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param include_referenced_posts Logical. Whether to include the posts in
#'   `includes$tweets` (the posts that were quoted, replied to or reposted).
#'   Defaults to TRUE.
#' @return A tibble with one row per earlier version per edited post and the
#'   character columns `post_id` and `edited_post_id`. A timeline without
#'   edited posts gives zero rows with the same columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_edited_post_id <- extract_post_edited_post_id(timeline)
#' }
#' @export
extract_post_edited_post_id <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  edit_schema <- tibble(
    post_id        = character(0),
    edited_post_id = character(0)
  )

  posts <- unique_posts(timeline, include_referenced_posts)

  bind_rows(edit_schema, map(posts, edit_rows)) |>
    filter(edited_post_id != post_id) |>
    distinct()
}

#' The edit history rows of one post, or NULL when it has none
#' @keywords internal
#' @noRd
edit_rows <- function(x) {
  history <- x$edit_history_tweet_ids
  if (is.null(history)) return(NULL)

  tibble(
    post_id        = x$id,
    edited_post_id = as.character(unlist(history))
  )
}
