#' Get Recent Post Count
#'
#' @description
#' Returns Post Counts from the last 7 days that match a search query via the
#' [recent posts count
#' endpoint](https://docs.x.com/x-api/posts/recent-search-counts).
#'
#' A count request is billed once (US$0.005 in September 2026), however
#' many posts it counts. The endpoint returns how many posts matched in each
#' period and no posts at all, so no post is billed. Use it to size a query
#' before paying for `get_recent_post()`.
#'
#' @importFrom purrr pluck map_chr map_int
#' @importFrom tibble tibble
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom dplyr mutate
#' @param query The search to be made on X. You can find ways to build
#'   specific queries according to the [X API documentation
#'   website](https://docs.x.com/x-api/posts/search/integrate/build-a-query#types)
#' @param start_time The earliest date-time from which you want to get posts.
#' @param end_time The latest date-time from which you want to get posts.
#'   Provide the value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
#'   `iso_8601()` function will convert a string, date, or date-time object to
#'   the required format (e.g., `iso_8601("2024-10-10")`).
#' @param granularity The period each count covers: `"minute"`, `"hour"` or
#'   `"day"`. The function stops before any request on anything else.
#' @param is_local_tz Logical. Convert `start` and `end` from UTC to the
#'   system time zone.
#' @param drop_incomplete Drops the periods the search window cuts short:
#'   any period shorter than `granularity`, or reaching outside
#'   `start_time` and `end_time`. A window that starts and ends on a period
#'   boundary (midnight for `"day"`) loses nothing.
#' @template bearer_token
#' @return A tibble with one row per period: `start` and `end` (date-times)
#'   and `post_count` (integer). When the query matched nothing, the tibble
#'   has the same three columns and no rows.
#' @examples
#' \dontrun{
#' counts <- get_recent_post_count("Developers")
#' }
#' @export
get_recent_post_count <- function(
  query,
  start_time       = NULL,
  end_time         = NULL,
  granularity      = "hour",
  is_local_tz      = TRUE,
  drop_incomplete  = TRUE,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN")
) {

  check_token(bearer_token)
  check_query(query)
  granularity <- check_granularity(granularity)
  announce_request_cost("counts_recent")

  this_response <- x_request(bearer_token) |>
    req_url_path_append("tweets", "counts", "recent") |>
    req_url_query(
      query       = query,
      end_time    = end_time,
      start_time  = start_time,
      granularity = granularity
    ) |>
    x_perform()

  counts <- pluck(this_response, "data") %||% list()

  counts_df <- tibble(
    start      = ymd_hms(map_chr(counts, "start", .default = NA_character_), tz = "UTC"),
    end        = ymd_hms(map_chr(counts, "end", .default = NA_character_), tz = "UTC"),
    post_count = map_int(counts, "tweet_count", .default = NA_integer_)
  )

  # Sorted and cut in UTC, before any change of time zone.
  counts_df <- tidy_count_periods(
    counts_df, granularity, start_time, end_time, drop_incomplete
  )

  if (isTRUE(is_local_tz)) {
    counts_df <- counts_df |>
      mutate(
        start = with_tz(start, tzone = Sys.timezone()),
        end   = with_tz(end, tzone = Sys.timezone())
      )
  }

  counts_df
}
