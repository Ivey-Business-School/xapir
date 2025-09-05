#' Get Trends by WOEID
#'
#' @description
#' Retrieves trending topics for a specified location via its WOEID from the
#' [Get Trends by WOEID endpoint](https://docs.x.com/x-api/trends/get-trends-by-woeid).
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_path_append req_url_query req_perform resp_body_json
#' @importFrom tibble tibble
#' @importFrom purrr map_chr map_int
#' @template bearer_token
#' @param woeid Integer WOEID of the location to fetch trends for.
#' @param max_trends Integer for maximum results (1–50, default 20).
#' @param trend_fields Character vector of fields to include (e.g., "trend_name", "tweet_count").
#' @return A tibble with trend names and tweet count, or NULL if no data.
#' @examples
#' \dontrun{
#' tr <- get_trends_by_woeid(woeid = 4118)  # e.g., Toronto
#' }
#' @export
get_trends_by_woeid <- function(
  woeid,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  max_trends = 20,
  trend_fields = c("trend_name", "tweet_count")
) {

  # Validate inputs
  if (!is.numeric(woeid) || length(woeid) != 1) {
    stop("`woeid` must be a single numeric value.")
  }
  if (!is.numeric(max_trends) || max_trends < 1 || max_trends > 50) {
    stop("`max_trends` must be between 1 and 50.")
  }

  # Construct the URL and parameters
  endpoint <- paste0("trends/by/woeid/", woeid)
  query <- list(
    max_trends = max_trends,
    trend.fields = paste(trend_fields, collapse = ",")
  )

  # Perform the request
  resp <- request(base_url = "https://api.x.com/2") |>
    req_url_path_append(endpoint = endpoint) |>
    req_auth_bearer_token(bearer_token) |>
    req_url_query(!!!query) |>
    req_perform() |>
    resp_body_json()

  # Handle errors from API
  if (!is.null(resp$errors)) {
    stop("API error: ", resp$errors[[1]]$detail %||% "Unknown error")
  }

  # Extract and return data as tibble
  if (is.null(resp$data) || length(resp$data) == 0) {
    message("No trending data available for WOEID: ", woeid)
    return(NULL)
  }

  trends <- tibble(
    trend_name  = map_chr(resp$data, ~ .x$trend_name %||% NA_character_),
    tweet_count = map_int(resp$data, ~ .x$tweet_count %||% NA_integer_)
  )

  return(trends)
}
