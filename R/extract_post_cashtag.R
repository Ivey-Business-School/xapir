#' Extract Post Cashtag Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle the
#' cashtags in each post (ticker symbols such as `$TSLA`) and where they sit
#' in the text. Each row is one cashtag in one post.
#'
#' Each post appears once, even when it sits in one page's `data` and another
#' page's `includes$tweets`. The `data` copy wins.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows distinct
#' @importFrom tibble tibble
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param include_referenced_posts Logical. Whether to include the posts in
#'   `includes$tweets` (the posts that were quoted, replied to or reposted).
#'   Defaults to TRUE.
#' @return A tibble with one row per cashtag per post and the columns
#'   `post_id` (character), `tag` (character, without the `$`), `start` and
#'   `end` (integer positions in the post text). A timeline without cashtags
#'   gives zero rows with the same columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_cashtag <- extract_post_cashtag(timeline)
#' }
#' @export
extract_post_cashtag <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  cashtag_schema <- tibble(
    post_id = character(0),
    tag     = character(0),
    start   = integer(0),
    end     = integer(0)
  )

  posts <- unique_posts(timeline, include_referenced_posts)

  bind_rows(cashtag_schema, map(posts, cashtag_rows)) |>
    distinct()
}

#' The cashtag rows of one post, or NULL when it has none
#' @keywords internal
#' @noRd
cashtag_rows <- function(x) {
  tags <- x$entities$cashtags
  if (is.null(tags)) return(NULL)

  # The argument is not called `tag`: inside tibble() the `tag` column would
  # shadow it, so `tag$start` would read the column instead of the list.
  bind_rows(map(tags, function(ct) {
    tibble(
      post_id = x$id,
      tag     = ct$tag %||% NA_character_,
      start   = ct$start %||% NA_integer_,
      end     = ct$end %||% NA_integer_
    )
  }))
}
