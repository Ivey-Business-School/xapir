#' Get Recent Post Count
#'
#' @description
#' Returns Post Counts from the last 7 days that match a search query via the
#' [recent posts count
#' endpoint](https://docs.x.com/x-api/posts/recent-search-counts).
#'
#' A count request is billed once (US\$0.005 in September 2026), however
#' many posts it counts. The endpoint returns how many posts matched in each
#' period and no posts at all, so nothing in this call is billed. Use it to
#' size a query before paying for `get_recent_post()`.
#'
#' @importFrom purrr pluck map_chr map_int
#' @importFrom tibble tibble
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom dplyr mutate slice n
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
#' @param drop_incomplete Drops the first and last rows, which cover only part
#'   of a `granularity` period.
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

  if (isTRUE(is_local_tz)) {
    counts_df <- counts_df |>
      mutate(
        start = with_tz(start, tzone = Sys.timezone()),
        end   = with_tz(end, tzone = Sys.timezone())
      )
  }

  # The first and last periods are cut by the search window, so their counts
  # are low. Nothing to drop when there are no rows.
  if (isTRUE(drop_incomplete) && nrow(counts_df) > 0) {
    counts_df <- counts_df |>
      slice(-1, -n())
  }

  counts_df
}
