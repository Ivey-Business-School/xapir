#' Extract Post Place and Geo Coordinates from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle place information
#' and exact geo-coordinates attached to posts.
#'
#' @importFrom purrr map map_dfr pluck flatten_chr compact
#' @importFrom dplyr mutate select any_of distinct
#' @importFrom tibble tibble
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured place and coordinate data.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_places <- extract_post_place(timeline)
#' }
#' @export
extract_post_place <- function(timeline) {

  post_place_map <- timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        place_id <- .x$geo$place_id %||% NA_character_

        tibble(
          post_id  = .x$id,
          place_id = as.character(place_id)
        )
      }
    ) |>
    filter(!is.na(place_id)) 

  # Extract place data directly
  timeline |>
    map(pluck("includes")) |>
    map(pluck("places")) |>
    unlist(recursive = FALSE) ->
    place_list

  place_tbl <- place_list |>
    map_dfr(
      ~ {
        bbox <- .x$geo$bbox %||% rep(NA, 4)
        
        tibble(
          place_id       = .x$id,
          full_name      = .x$full_name %||% NA_character_,
          country        = .x$country %||% NA_character_,
          country_code   = .x$country_code %||% NA_character_,
          place_type     = .x$place_type %||% NA_character_,
          west_longitude = bbox[[1]],
          south_latitude = bbox[[2]],
          east_longitude = bbox[[3]],
          north_latitude = bbox[[4]]
        )
      }
    )

  # Join place details to post table
  post_places <- post_place_map |>
    left_join(place_tbl, by = "place_id") |>
    select(post_id, place_id, full_name, country, country_code, place_type,
      west_longitude, south_latitude, east_longitude, north_latitude) |>
    distinct()

  return(post_places)
}
