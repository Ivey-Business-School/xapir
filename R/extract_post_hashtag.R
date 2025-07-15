#' Extract Post Hashtag Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle post hashtag
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
#' post <- extract_post_hashtag(timeline)
#' }
#' @export
extract_post_hashtag <- function(
    timeline
) {
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
        ~ {
            if (!is.null(.x$entities$hashtags)) {
                map_dfr(.x$entities$hashtags, function(tag) {
                    tibble(
                        post_id  = .x$id,
                        hashtag  = tag[["tag"]] %||% NA_character_,
                        start    = tag[["start"]] %||% NA_integer_,
                        end      = tag[["end"]] %||% NA_integer_
                    )
                })
            } else {
                tibble(
                    post_id = character(0),
                    hashtag = character(0),
                    start   = integer(0),
                    end     = integer(0)
                )
            }
        }
    ) |>
    distinct() -> 
    post_hashtag 

    return(post_hashtag)
}
