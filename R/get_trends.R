#' Get Trends by WOEID
#'
#' @description
#' Retrieves the trending topics for a location, given its WOEID (Yahoo's
#' "Where On Earth" id), via the
#' [get trends by WOEID endpoint](https://docs.x.com/x-api/trends/get-trends-by-woeid).
#' For example, 1 is worldwide, 23424977 is the United States and 4118 is
#' Toronto.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr map_chr
#' @param woeid The location's WOEID, one number or a string of digits.
#' @template bearer_token
#' @param max_trends The most trends to return, between 1 and 50. Default 20.
#' @param trend_fields \code{character}, \code{vector}; the fields to return
#'   for each trend. The API calls the post count `tweet_count`.
#' @return A tibble with one row per trend: `trend_name` and `post_count`
#'   (the number of posts on the topic, when the API reports one). A location
#'   with no trends gives the same columns with no rows.
#' @examples
#' \dontrun{
#' trends <- get_trends_by_woeid(woeid = 4118)  # Toronto
#' }
#' @export
get_trends_by_woeid <- function(
  woeid,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  max_trends   = 20,
  trend_fields = c("trend_name", "tweet_count")
) {
  check_token(bearer_token)

  woeid_ok <- length(woeid) == 1 && !is.na(woeid) &&
    grepl("^[0-9]+$", as.character(woeid))
  if (!woeid_ok) {
    stop(
      "`woeid` must be one whole number, such as 1 for worldwide.",
      call. = FALSE
    )
  }
  max_trends_ok <- is.numeric(max_trends) && length(max_trends) == 1 &&
    !is.na(max_trends) && max_trends >= 1 && max_trends <= 50
  if (!max_trends_ok) {
    stop("`max_trends` must be a number between 1 and 50.", call. = FALSE)
  }

  announce_request_cost("trends")

  page <- x_request(bearer_token) |>
    req_url_path_append("trends", "by", "woeid", as.character(woeid)) |>
    req_url_query(
      max_trends   = as.integer(max_trends),
      trend.fields = join_fields(trend_fields)
    ) |>
    x_perform()

  if (is.null(page$data) && length(page$errors) > 0) {
    reason <- map_chr(page$errors, ~ .x$detail %||% .x$title %||% "")
    stop(
      "No trends found for woeid ", woeid, ". ",
      paste(reason, collapse = " "),
      call. = FALSE
    )
  }

  trends_table(page$data)
}
