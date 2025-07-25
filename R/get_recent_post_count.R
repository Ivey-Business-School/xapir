#' Get Recent Post Count
#'
#' @description
#' Returns Post Counts from the last 7 days that match a search query via the [recent posts count 
#' endpoint](https://docs.x.com/x-api/posts/recent-search-counts).
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_path_append req_perform resp_body_json req_url_query
#' @importFrom purrr pluck
#' @importFrom tibble tibble
#' @importFrom lubridate ymd_hms
#' @param query The search to be made on X. You can find ways to build specific queries according to the [X API documentation website](https://docs.x.com/x-api/posts/search/integrate/build-a-query#types)
#' @param start_time The earliest date-time from which you want to get posts.
#' @param end_time The latest date-time from which you want to get posts.
#' Provide the value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
#'   `iso_8601()` function will convert a string, date, or date-time object to
#'   the required format (e.g., `iso_8601("2024-10-10")`).
#' @param granularity The granularity for the search count results. This takes either the value 'minute', 'hour', or 'day'.
#' @param is_local_tz Tracks wheather to use the local timezone or default timezone.
#' @param drop_incomplete Drops rows that do not contain a full granularity period amount of data.
#' @template bearer_token
#' @return A tibble containing the number of posts.
#' @examples
#' \dontrun{
#' tl <- get_recent_post_count("Developers")
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

  # Make the API request
  while (TRUE) {
    tryCatch(
      expr = {
        request("https://api.x.com/2/tweets/counts/recent") |>
          req_url_query(
            query       = query,
            end_time    = end_time,
            start_time  = start_time,
            granularity = granularity
          ) |>
          req_auth_bearer_token(token = bearer_token) |>
          req_perform() |>
          resp_body_json() ->
          this_response

        # Exit the loop if successful
        break
      },
      error = function(e) {
        message(e$message, " Retrying in 60 seconds.")
        Sys.sleep(60)
      }
    )
  }

  # Extract per-interval counts
  counts <- pluck(this_response, "data", .default = NULL)

  if (is.null(counts)) {
    message("No count data found.")
    return(NULL)
  }

  counts_df <- tibble(
    start = ymd_hms(sapply(counts, `[[`, "start")),
    end = ymd_hms(sapply(counts, `[[`, "end")),
    post_count = sapply(counts, `[[`, "tweet_count")
  )

  if (is_local_tz == TRUE) {
    counts_df <- counts_df |>
      mutate(
        start = with_tz(start, tzone = Sys.timezone()),
        end   = with_tz(end, tzone = Sys.timezone())
      )
  }

  if (drop_incomplete == TRUE) {
    counts_df <- counts_df |>
      slice(-1, -n())
  }
  
  return(counts_df)
}
