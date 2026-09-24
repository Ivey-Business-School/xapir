#' Extract Post Place and Geo Coordinates from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle the place
#' a post was tagged with, from `includes$places`, and its bounding box.
#' Each row is one post with a place tag. When the place's details are not
#' in `includes`, the row keeps its `post_id` and `place_id` and the other
#' columns are `NA`.
#'
#' Each post appears once, even when it sits in one page's `data` and another
#' page's `includes$tweets`. The `data` copy wins.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows left_join select all_of distinct
#' @importFrom tibble tibble
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param include_referenced_posts Logical. Whether to include the posts in
#'   `includes$tweets` (the posts that were quoted, replied to or reposted).
#'   Defaults to TRUE.
#' @return A tibble with one row per tagged post and the columns `post_id`,
#'   `place_id`, `full_name`, `country`, `country_code` and `place_type`
#'   (character), and `west_longitude`, `south_latitude`, `east_longitude`
#'   and `north_latitude` (numeric, the place's bounding box). A timeline
#'   without place tags gives zero rows with the same columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_place <- extract_post_place(timeline)
#' }
#' @export
extract_post_place <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  place_schema <- tibble(
    post_id        = character(0),
    place_id       = character(0),
    full_name      = character(0),
    country        = character(0),
    country_code   = character(0),
    place_type     = character(0),
    west_longitude = numeric(0),
    south_latitude = numeric(0),
    east_longitude = numeric(0),
    north_latitude = numeric(0)
  )

  # Step 1: which place is each post tagged with?
  posts <- unique_posts(timeline, include_referenced_posts)

  post_place_map <- bind_rows(
    select(place_schema, post_id, place_id),
    map(posts, function(x) {
      place_id <- x$geo$place_id
      if (is.null(place_id)) return(NULL)
      tibble(post_id = x$id, place_id = as.character(place_id))
    })
  )

  # Step 2: the details of every place in includes$places, each once
  place_tbl <- bind_rows(
    select(place_schema, -post_id),
    map(unique_includes(timeline, "places"), place_row)
  )

  # Step 3: attach the details to each tagged post
  post_place_map |>
    left_join(place_tbl, by = "place_id") |>
    select(all_of(names(place_schema))) |>
    distinct()
}

#' One row of place details from one item of includes$places
#' @keywords internal
#' @noRd
place_row <- function(x) {
  bbox <- as.numeric(unlist(x$geo$bbox))
  if (length(bbox) != 4) bbox <- rep(NA_real_, 4)

  tibble(
    place_id       = x$id %||% NA_character_,
    full_name      = x$full_name %||% NA_character_,
    country        = x$country %||% NA_character_,
    country_code   = x$country_code %||% NA_character_,
    place_type     = x$place_type %||% NA_character_,
    west_longitude = bbox[[1]],
    south_latitude = bbox[[2]],
    east_longitude = bbox[[3]],
    north_latitude = bbox[[4]]
  )
}
