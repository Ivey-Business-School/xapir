#' Extract Post Cashtag Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle post cashtag data,
#' including metadata relating to the ticker symbol and its position in the post.
#'
#' @importFrom purrr map map_dfr pluck map_chr
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
#' post <- extract_post_cashtag(timeline)
#' }
#' @export
extract_post_cashtag <- function(
    timeline
) {
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(~ {
      if (!is.null(.x$entities$cashtags)) {
        map_dfr(.x$entities$cashtags, function(cashtag) {
          tag_val   <- if (is.list(cashtag) && !is.null(cashtag[["tag"]]))   cashtag[["tag"]]   else NA_character_
          start_val <- if (is.list(cashtag) && !is.null(cashtag[["start"]])) cashtag[["start"]] else NA_integer_
          end_val   <- if (is.list(cashtag) && !is.null(cashtag[["end"]]))   cashtag[["end"]]   else NA_integer_

          tibble(
            post_id  = .x$id,
            tag      = tag_val,
            start    = start_val,
            end      = end_val
          )
        })
      } else {
        tibble(post_id = character(0), tag = character(0), start = integer(0), end = integer(0))
      }
    }) |>
    distinct() ->
    post_cashtag

    return(post_cashtag)
}
