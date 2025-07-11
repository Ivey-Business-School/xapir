#' Extract Post Context Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle post context data,
#' including metadata relating to the domain and entity.
#'
#' @importFrom purrr map map_dfr map_chr pluck map_lgl
#' @importFrom dplyr mutate select any_of arrange distinct
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured post context data.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post <- extract_post_context(timeline)
#' }
#' @export
extract_post_context <- function(
  timeline
) {
  timeline |>
    map(pluck("data")) |> 
    unlist(recursive = FALSE) |>
    map_dfr(
        ~ {
            if (!is.null(.x$context_annotations)) {
                tibble(
                    tweet_id           = .x$id,
                    domain_id          = map_chr(.x$context_annotations, ~ .x$domain$id %||% NA_character_),
                    domain_name        = map_chr(.x$context_annotations, ~ .x$domain$name %||% NA_character_),
                    domain_description = map_chr(.x$context_annotations, ~ .x$domain$description %||% NA_character_),
                    entity_id          = map_chr(.x$context_annotations, ~ .x$entity$id %||% NA_character_),
                    entity_name        = map_chr(.x$context_annotations, ~ .x$entity$name %||% NA_character_),
                    entity_description = map_chr(.x$context_annotations, ~ .x$entity$description %||% NA_character_)
                )
            } else {
                NULL
            }
        }
    ) |>
    distinct() ->
    post_context

    return(post_context)
}
