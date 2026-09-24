#' Extract Post URL Information from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle the links
#' in each post, such as the shortened `t.co` address, the address it expands
#' to, and the linked page's title and description. Each row is one link in
#' one post. The API lists the `pic.x.com` link of a post once per attached
#' photo; it is kept once here, since the media themselves are in
#' [extract_post_media()].
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
#' @return A tibble with one row per link per post and the columns `post_id`,
#'   `start`, `end`, `url`, `expanded_url`, `unwound_url`, `display_url`,
#'   `title`, `description`, `status` and `image_url`. `start`, `end` and
#'   `status` are integer; the rest are character. `image_url` is the first
#'   preview image of the linked page, or `NA`. A timeline without links gives
#'   zero rows with the same columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_url <- extract_post_url(timeline)
#' }
#' @export
extract_post_url <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  url_schema <- tibble(
    post_id      = character(0),
    start        = integer(0),
    end          = integer(0),
    url          = character(0),
    expanded_url = character(0),
    unwound_url  = character(0),
    display_url  = character(0),
    title        = character(0),
    description  = character(0),
    status       = integer(0),
    image_url    = character(0)
  )

  posts <- unique_posts(timeline, include_referenced_posts)

  bind_rows(url_schema, map(posts, url_rows)) |>
    distinct()
}

#' The link rows of one post, or NULL when it has no links
#' @keywords internal
#' @noRd
url_rows <- function(x) {
  urls <- x$entities$urls
  if (is.null(urls)) return(NULL)

  bind_rows(map(urls, function(u) {
    images <- u$images %||% list()
    tibble(
      post_id      = x$id,
      start        = u$start %||% NA_integer_,
      end          = u$end %||% NA_integer_,
      url          = u$url %||% NA_character_,
      expanded_url = u$expanded_url %||% NA_character_,
      unwound_url  = u$unwound_url %||% NA_character_,
      display_url  = u$display_url %||% NA_character_,
      title        = u$title %||% NA_character_,
      description  = u$description %||% NA_character_,
      status       = u$status %||% NA_integer_,
      image_url    = if (length(images) > 0) images[[1]]$url %||% NA_character_ else NA_character_
    )
  }))
}
