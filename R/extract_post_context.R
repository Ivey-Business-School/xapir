#' Extract Post Context Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle the
#' context annotations of each post: the domains (such as "Brand") and
#' entities (such as a company) the API tags the post with. Each row is one
#' annotation on one post.
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
#' @return A tibble with one row per context annotation per post and the
#'   character columns `post_id`, `domain_id`, `domain_name`,
#'   `domain_description`, `entity_id`, `entity_name` and
#'   `entity_description`. A timeline without context annotations gives zero
#'   rows with the same columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_context <- extract_post_context(timeline)
#' }
#' @export
extract_post_context <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  context_schema <- tibble(
    post_id            = character(0),
    domain_id          = character(0),
    domain_name        = character(0),
    domain_description = character(0),
    entity_id          = character(0),
    entity_name        = character(0),
    entity_description = character(0)
  )

  posts <- unique_posts(timeline, include_referenced_posts)

  bind_rows(context_schema, map(posts, context_rows)) |>
    distinct()
}

#' The context annotation rows of one post, or NULL when it has none
#' @keywords internal
#' @noRd
context_rows <- function(x) {
  annotations <- x$context_annotations
  if (is.null(annotations)) return(NULL)

  bind_rows(map(annotations, function(a) {
    tibble(
      post_id            = x$id,
      domain_id          = a$domain$id %||% NA_character_,
      domain_name        = a$domain$name %||% NA_character_,
      domain_description = a$domain$description %||% NA_character_,
      entity_id          = a$entity$id %||% NA_character_,
      entity_name        = a$entity$name %||% NA_character_,
      entity_description = a$entity$description %||% NA_character_
    )
  }))
}
