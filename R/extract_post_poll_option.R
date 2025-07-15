#' Extract Post Poll Option Information from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle poll option information,
#' such as poll IDs, options, and voting details.
#'
#' @importFrom purrr map map_dfr pluck compact flatten
#' @importFrom dplyr select distinct left_join
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom lubridate ymd_hms
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured poll data.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' polls <- extract_post_poll_option(timeline)
#' }
#' @export
extract_post_poll_option <- function(
  timeline
) {
  poll_columns <- c(
    "post_id", "poll_id", "position", "label", "votes",
    "duration_minutes", "end_datetime", "voting_status"
  )

  # Step 1: Map post_id to poll_id
  post_poll_map <- timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(~ {
      poll_ids <- .x$attachments$poll_ids %||% NULL
      if (is.null(poll_ids)) return(NULL)
      tibble(
        post_id = .x$id,
        poll_id = as.character(poll_ids)
      )
    }) 

  if (nrow(post_poll_map) == 0) {
    return(tibble(!!!setNames(rep(list(logical(0)), length(poll_columns)), poll_columns)))
  }

  post_poll_map <- unnest(post_poll_map, poll_id)

  # Step 2: Extract and flatten includes$polls
  poll_list <- timeline |>
    map("includes") |>
    map("polls") |>
    compact() |>  # remove NULLs
    flatten()

  if (length(poll_list) == 0) {
    return(tibble(!!!setNames(rep(list(logical(0)), length(poll_columns)), poll_columns)))
  }

  # Step 3: Create one row per poll option
  poll_tbl <- poll_list |>
    map_dfr(~ {
      poll_id <- .x$id
      duration <- .x$duration_minutes %||% NA
      end_time <- .x$end_datetime %||% NA_character_
      end_time <- if (!is.na(end_time)) ymd_hms(end_time, tz = "UTC") else as.POSIXct(NA)
      status   <- .x$voting_status %||% NA_character_

      if (!is.null(.x$options)) {
        map_dfr(.x$options, function(opt) {
          tibble(
            poll_id          = poll_id,
            position         = opt[["position"]] %||% NA_integer_,
            label            = opt[["label"]] %||% NA_character_,
            votes            = opt[["votes"]] %||% NA_integer_,
            duration_minutes = duration,
            end_datetime     = end_time,
            voting_status    = status
          )
        })
      } else {
        tibble(
          poll_id          = poll_id,
          position         = integer(0),
          label            = character(0),
          votes            = integer(0),
          duration_minutes = integer(0),
          end_datetime     = as.POSIXct(character(0)),
          voting_status    = character(0)
        )
      }
    })

  # Step 4: Join post_id with poll options
  post_polls <- left_join(poll_tbl, post_poll_map, by = "poll_id") |>
    select(any_of(poll_columns)) |>
    distinct()

  return(post_polls)
}
