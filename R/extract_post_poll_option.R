#' Extract Post Poll Option Information from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle poll
#' option information, such as poll IDs, options, and voting details. Each
#' row is one option of one poll on one post, so a two-option poll gives two
#' rows. A poll whose post is not in the response has nothing to attach to
#' and is left out.
#'
#' Each post appears once, even when it sits in one page's `data` and another
#' page's `includes$tweets`. The `data` copy wins.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows left_join filter select all_of distinct
#' @importFrom tibble tibble
#' @importFrom lubridate ymd_hms
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param include_referenced_posts Logical. Whether to include the posts in
#'   `includes$tweets` (the posts that were quoted, replied to or reposted).
#'   Defaults to TRUE.
#' @return A tibble with one row per poll option per post and the columns
#'   `post_id` and `poll_id` (character), `position` (integer, the option's
#'   order in the poll), `label` (character), `votes` (integer),
#'   `duration_minutes` (integer), `end_datetime` (POSIXct, UTC) and
#'   `voting_status` (character, "open" or "closed"). A timeline without polls
#'   gives zero rows with the same columns.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_poll_option <- extract_post_poll_option(timeline)
#' }
#' @export
extract_post_poll_option <- function(
  timeline,
  include_referenced_posts = TRUE
) {

  poll_schema <- tibble(
    post_id          = character(0),
    poll_id          = character(0),
    position         = integer(0),
    label            = character(0),
    votes            = integer(0),
    duration_minutes = integer(0),
    end_datetime     = as.POSIXct(character(0), tz = "UTC"),
    voting_status    = character(0)
  )

  # Step 1: which poll does each post carry?
  posts <- unique_posts(timeline, include_referenced_posts)

  post_poll_map <- bind_rows(
    select(poll_schema, post_id, poll_id),
    map(posts, function(x) {
      poll_ids <- x$attachments$poll_ids
      if (is.null(poll_ids)) return(NULL)
      tibble(post_id = x$id, poll_id = as.character(unlist(poll_ids)))
    })
  )

  # Step 2: one row per option of every poll in includes$polls, each poll once
  option_tbl <- bind_rows(
    select(poll_schema, -post_id),
    map(unique_includes(timeline, "polls"), poll_option_rows)
  )

  # Step 3: attach each poll's options to the post that carries it. A poll
  # whose post is not in the response has no post_id and is dropped.
  option_tbl |>
    left_join(post_poll_map, by = "poll_id", relationship = "many-to-many") |>
    filter(!is.na(post_id)) |>
    select(all_of(names(poll_schema))) |>
    distinct()
}

#' The option rows of one poll from includes$polls, or NULL when it has none
#' @keywords internal
#' @noRd
poll_option_rows <- function(x) {
  options <- x$options
  if (is.null(options)) return(NULL)

  end_time <- x$end_datetime %||% NA_character_
  end_time <- ymd_hms(end_time, tz = "UTC", quiet = TRUE)

  bind_rows(map(options, function(opt) {
    tibble(
      poll_id          = x$id %||% NA_character_,
      position         = opt$position %||% NA_integer_,
      label            = opt$label %||% NA_character_,
      votes            = opt$votes %||% NA_integer_,
      duration_minutes = x$duration_minutes %||% NA_integer_,
      end_datetime     = end_time,
      voting_status    = x$voting_status %||% NA_character_
    )
  }))
}
