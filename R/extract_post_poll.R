#' Extract Post Poll Information from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle poll information,
#' such as poll IDs, options, and voting details.
#'
#' @importFrom purrr map map_dfr pluck flatten_chr compact
#' @importFrom dplyr mutate select any_of distinct
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured poll data.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' polls <- extract_post_poll(timeline)
#' }
#' @export
extract_post_poll <- function(timeline) {

  post_poll_map <- timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        keys <- .x$attachments$poll_ids %||% NULL
        if (is.null(keys)) return(NULL)
        tibble(
          post_id = .x$id,
          poll_id = as.character(keys)  # ensure character vector
        )
      }
    ) |>
    unnest(poll_id)

  # Extract poll data directly
  timeline |>
    map(pluck("includes")) |>
    map(pluck("polls")) |>
    unlist(recursive = FALSE) ->
    poll_list

  # Create the poll tibble
  poll_tbl <- poll_list |>
    map_dfr(
      ~ {
        options <- .x$options %||% list()
        tibble(
          poll_id = .x$id,
          duration_minutes = .x$duration_minutes %||% NA |> as.character(),
          end_datetime = .x$end_datetime %||% NA |> as.character(),
          voting_status = .x$voting_status %||% NA |> as.character(),
          option_labels = paste0(map_chr(options, ~ .x$label), collapse = "; "),
          option_votes = paste0(map_chr(options, ~ as.character(.x$votes %||% NA)), collapse = "; ")
        )
      }
    )

  # Join post_id to poll table
  post_polls <- poll_tbl |>
    left_join(post_poll_map, by = "poll_id") |>
    select(post_id, poll_id, duration_minutes, end_datetime, voting_status, option_labels, option_votes) |>
    distinct()

  return(post_polls)
}
