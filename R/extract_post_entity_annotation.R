#' Extract Post Entity Annotation Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle post entity annotation 
#' data, including metadata relating to the type and its probability.
#'
#' @importFrom purrr map map_dfr pluck
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured post context data.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post <- extract_post_entity_annotation(timeline)
#' }
#' @export
extract_post_entity_annotation <- function(
    timeline
) {
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
        ~ {
            if (!is.null(.x$entities$annotations)) {
                map_dfr(.x$entities$annotations, function(annotation) {
                    tibble(
                        post_id          = .x$id,
                        normalized_text  = annotation[["normalized_text"]] %||% NA_character_,
                        type             = annotation[["type"]] %||% NA_character_,
                        probability      = annotation[["probability"]] %||% NA_real_,
                        start            = annotation[["start"]] %||% NA_integer_,
                        end              = annotation[["end"]] %||% NA_integer_
                    )
                })
            } else {
                tibble(
                    post_id         = character(0),
                    normalized_text = character(0),
                    type            = character(0),
                    probability     = numeric(0),
                    start           = integer(0),
                    end             = integer(0)
                )
            }
        }
    ) |>
    distinct() ->
    post_entity_annotation

    return(post_entity_annotation)
}
