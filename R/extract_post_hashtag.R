#' Extract Post Hashtag Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle the
#' hashtags in each post and where they sit in the text. Each row is one
#' hashtag in one post.
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
#' @return A tibble with one row per hashtag per post and the columns
#'   `post_id` (character), `hashtag` (character, without the `#`), `start`
#'   and `end` (integer positions in the post text). A timeline without
#'   hashtags gives zero rows with the same columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_hashtag <- extract_post_hashtag(timeline)
#' }
#' @export
extract_post_hashtag <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  hashtag_schema <- tibble(
    post_id = character(0),
    hashtag = character(0),
    start   = integer(0),
    end     = integer(0)
  )

  posts <- unique_posts(timeline, include_referenced_posts)

  bind_rows(hashtag_schema, map(posts, hashtag_rows)) |>
    distinct()
}

#' The hashtag rows of one post, or NULL when it has none
#' @keywords internal
#' @noRd
hashtag_rows <- function(x) {
  tags <- x$entities$hashtags
  if (is.null(tags)) return(NULL)

  bind_rows(map(tags, function(h) {
    tibble(
      post_id = x$id,
      hashtag = h$tag %||% NA_character_,
      start   = h$start %||% NA_integer_,
      end     = h$end %||% NA_integer_
    )
  }))
}
