#' Extract Post Entity Annotation Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle the entity
#' annotations of each post: the people, places, products and organizations
#' the API recognises in the text, with the API's confidence and where the
#' words sit in the text. Each row is one annotation on one post.
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
#' @return A tibble with one row per entity annotation per post and the
#'   columns `post_id`, `normalized_text` and `type` (character),
#'   `probability` (numeric, 0 to 1), and `start` and `end` (integer positions
#'   in the post text). A timeline without entity annotations gives zero rows
#'   with the same columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_entity_annotation <- extract_post_entity_annotation(timeline)
#' }
#' @export
extract_post_entity_annotation <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  annotation_schema <- tibble(
    post_id         = character(0),
    normalized_text = character(0),
    type            = character(0),
    probability     = numeric(0),
    start           = integer(0),
    end             = integer(0)
  )

  posts <- unique_posts(timeline, include_referenced_posts)

  bind_rows(annotation_schema, map(posts, entity_annotation_rows)) |>
    distinct()
}

#' The entity annotation rows of one post, or NULL when it has none
#' @keywords internal
#' @noRd
entity_annotation_rows <- function(x) {
  annotations <- x$entities$annotations
  if (is.null(annotations)) return(NULL)

  bind_rows(map(annotations, function(a) {
    tibble(
      post_id         = x$id,
      normalized_text = a$normalized_text %||% NA_character_,
      type            = a$type %||% NA_character_,
      probability     = a$probability %||% NA_real_,
      start           = a$start %||% NA_integer_,
      end             = a$end %||% NA_integer_
    )
  }))
}
