#' Extract Post URL Information from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle poll URL information,
#' such as the URL, title, and description. 
#'
#' @importFrom purrr map map_dfr pluck 
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured poll data.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' polls <- extract_post_url(timeline)
#' }
#' @export
extract_post_url <- function(
    timeline
) {
  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
        ~ {
            if (!is.null(.x$entities$urls)) {
                map_dfr(.x$entities$urls, function(u) {
                    tibble(
                        post_id       = .x$id,
                        start         = u[["start"]] %||% NA_integer_,
                        end           = u[["end"]] %||% NA_integer_,
                        url           = u[["url"]] %||% NA_character_,
                        expanded_url  = u[["expanded_url"]] %||% NA_character_,
                        unwound_url   = u[["unwound_url"]] %||% NA_character_,
                        display_url   = u[["display_url"]] %||% NA_character_,
                        title         = u[["title"]] %||% NA_character_,
                        description   = u[["description"]] %||% NA_character_,
                        status        = u[["status"]] %||% NA_integer_,
                        image_url     = if (!is.null(u[["images"]]) && length(u[["images"]]) > 0)
                                        u[["images"]][[1]][["url"]] %||% NA_character_
                                        else
                                        NA_character_
                    )
                })
            } else {
                tibble(
                    post_id       = character(0),
                    start         = integer(0),
                    end           = integer(0),
                    url           = character(0),
                    expanded_url  = character(0),
                    unwound_url   = character(0),
                    display_url   = character(0),
                    title         = character(0),
                    description   = character(0),
                    status        = integer(0),
                    image_url     = character(0)
                )
            }
        }
    ) |>
    distinct() -> 
    post_url

    return(post_url)
}
