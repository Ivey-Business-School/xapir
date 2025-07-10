#' Extract Post Edited Post ID from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to retrieve data on previous
#' versions of posts. 
#'
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured edited post ID data.
#' @importFrom purrr map pluck map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
#' @importFrom tidyr unnest
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
          tweet_id          = .x$id,
          history_tweet_id  = .x$edit_history_tweet_ids
        )
      } else {
        NULL
      }
    }) |>
    unnest(cols = history_tweet_id) |>
    distinct() ->
    post_edited_post_id

    return(post_edited_post_id)
}
