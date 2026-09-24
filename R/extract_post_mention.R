#' Extract Post Mention Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle the users
#' mentioned in each post and where the mention sits in the text. Each row is
#' one mention in one post.
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
#' @return A tibble with one row per mention per post and the columns
#'   `post_id`, `username` and `user_id` (character), and `start` and `end`
#'   (integer positions in the post text). A timeline without mentions gives
#'   zero rows with the same columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_mention <- extract_post_mention(timeline)
#' }
#' @export
extract_post_mention <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  mention_schema <- tibble(
    post_id  = character(0),
    username = character(0),
    user_id  = character(0),
    start    = integer(0),
    end      = integer(0)
  )

  posts <- unique_posts(timeline, include_referenced_posts)

  bind_rows(mention_schema, map(posts, mention_rows)) |>
    distinct()
}

#' The mention rows of one post, or NULL when it has none
#' @keywords internal
#' @noRd
mention_rows <- function(x) {
  mentions <- x$entities$mentions
  if (is.null(mentions)) return(NULL)

  bind_rows(map(mentions, function(m) {
    tibble(
      post_id  = x$id,
      username = m$username %||% NA_character_,
      user_id  = m$id %||% NA_character_,
      start    = m$start %||% NA_integer_,
      end      = m$end %||% NA_integer_
    )
  }))
}
