#' Extract Post Mention Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle post mention
#' data.
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
#' post <- extract_post_mention(timeline)
#' }
#' @export
extract_post_mention <- function(
    timeline
) {
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
        ~ {
            if (!is.null(.x$entities$mentions)) {
                map_dfr(.x$entities$mentions, function(mention) {
                    tibble(
                        tweet_id = .x$id,
                        username = mention[["username"]] %||% NA_character_,
                        user_id  = mention[["id"]] %||% NA_character_,
                        start    = mention[["start"]] %||% NA_integer_,
                        end      = mention[["end"]] %||% NA_integer_
                    )
                })
            } else {
                NULL
            }
        }
    ) |>
    distinct() -> 
    post_mention

    return(post_mention)
}
