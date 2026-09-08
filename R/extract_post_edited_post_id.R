#' Extract Post Edited Post ID from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to retrieve data on previous
#' versions of posts. The API lists a post's own id in its edit history, so an
#' unedited post lists only itself. Those self rows are dropped: a row here
#' means the post was edited, and `edited_post_id` is an earlier version.
#'
#' @importFrom purrr map pluck map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr distinct filter
#' @importFrom tidyr unnest
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured edited post ID data.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post <- extract_post_edited_post_id(timeline)
#' }
#' @export
extract_post_edited_post_id <- function(
    timeline
) {
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(~ {
      if (!is.null(.x$edit_history_tweet_ids)) {
        tibble(
          post_id          = .x$id,
          edited_post_id   = .x$edit_history_tweet_ids
        )
      } else {
        tibble(
          post_id        = character(0),
          edited_post_id = list()
        )
      }
    }) |>
    unnest(cols = edited_post_id) |>
    filter(edited_post_id != post_id) |>
    distinct() ->
    post_edited_post_id

    return(post_edited_post_id)
}
