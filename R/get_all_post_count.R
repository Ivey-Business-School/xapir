#' Get All Post Count
#'
#' @description
#' Returns how many posts in the full archive, back to 2006, match a search
#' query in each period via the [full-archive post counts
#' endpoint](https://docs.x.com/x-api/posts/get-count-of-all-posts). The
#' endpoint needs pay-per-use or Enterprise access; on a tier without it the
#' call stops with the API's own message.
#'
#' A count request is billed once (US$0.010 in September 2026), however many
#' posts it counts, and returns no posts at all. A long range comes back in
#' pages; the function follows them until the API has no more and prints how
#' many requests it made. Use it to size a query before paying for
#' [get_all_post()].
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr pluck map_chr map_int
#' @importFrom tibble tibble
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom dplyr mutate slice n bind_rows
#' @param query The search to be made on X. You can find ways to build
#'   specific queries according to the [X API documentation
#'   website](https://docs.x.com/x-api/posts/search/integrate/build-a-query#types)
#' @param start_time The earliest date-time from which you want to count
#'   posts. Provide the value in ISO 8601 format (i.e.,
#'   `YYYY-MM-DDTHH:mm:ssZ`). The `iso_8601()` function will convert a
#'   string, date, or date-time object to the required format (e.g.,
#'   `iso_8601("2024-10-10")`). Without it the count starts 30 days ago.
#' @param end_time The latest date-time to which you want to count posts.
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
#' counts <- get_all_post_count(
#'   "#SuperBowl lang:en",
#'   start_time = iso_8601("2020-01-01"),
#'   end_time = iso_8601("2020-03-01")
#' )
#' }
#' @export
get_all_post_count <- function(
  query,
  start_time       = NULL,
  end_time         = NULL,
  granularity      = "day",
  is_local_tz      = TRUE,
  drop_incomplete  = TRUE,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN")
) {
  check_token(bearer_token)
  check_query(query)
  granularity <- check_granularity(granularity)
  announce_request_cost("counts_all")

  req <- x_request(bearer_token) |>
    req_url_path_append("tweets", "counts", "all") |>
    req_url_query(
      query       = query,
      start_time  = start_time,
      end_time    = end_time,
      granularity = granularity
    )

  # A long range is paged. Each page is one billed request, so the total is
  # announced once the last page is in.
  counts <- list()
  next_token <- NULL
  n_requests <- 0

  repeat {
    page <- req |>
      req_url_query(next_token = next_token) |>
      x_perform()
    n_requests <- n_requests + 1

    warn_partial_errors(page$errors, what = "counts")
    counts <- c(counts, pluck(page, "data") %||% list())

    next_token <- pluck(page, "meta", "next_token")
    if (is.null(next_token)) {
      break
    }
  }

  if (n_requests > 1) {
    announce_request_cost("counts_all", n = n_requests)
  }

  # The archive endpoint names the count `post_count`; older responses used
  # `tweet_count`. Either is read.
  counts_df <- tibble(
    start      = ymd_hms(map_chr(counts, "start", .default = NA_character_), tz = "UTC"),
    end        = ymd_hms(map_chr(counts, "end", .default = NA_character_), tz = "UTC"),
    post_count = map_int(counts, function(x) {
      as.integer(x$post_count %||% x$tweet_count %||% NA_integer_)
    })
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
