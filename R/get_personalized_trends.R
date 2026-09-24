#' Get Personalized Trends
#'
#' @description
#' Retrieves the trends X picks for the signed-in account via the [get
#' personalized trends
#' endpoint](https://docs.x.com/x-api/trends/get-personalized-trends). Needs
#' a user token, so the first call opens a browser window to sign in. The
#' request is billed, so the function says what it costs before it reads
#' anything.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr keep map_chr map_dfr
#' @return A tibble with one row per trend: `trend_name`, `category`,
#'   `post_count` (integer, `NA` when the API gives a count it cannot parse
#'   or none at all) and `trending_since` (the API's own text). When there
#'   are no trends, the same columns with no rows.
#' @examples
#' \dontrun{
#' trends <- get_personalized_trends()
#' }
#' @export
get_personalized_trends <- function() {

  token <- authenticate_user()

  announce_request_cost("trends")

  page <- x_request(token$access_token) |>
    req_url_path_append("users", "personalized_trends") |>
    req_url_query(
      personalized_trend.fields =
        "category,post_count,trend_name,trending_since"
    ) |>
    x_perform()

  if (is.null(page$data) && length(page$errors) > 0) {
    reason <- map_chr(page$errors, ~ .x$detail %||% .x$title %||% "")
    stop("No trends returned. ", paste(reason, collapse = " "), call. = FALSE)
  }

  personalized_trends_table(page$data)
}

personalized_trend_schema <- function() {
  tibble(
    trend_name     = character(0),
    category       = character(0),
    post_count     = integer(0),
    trending_since = character(0)
  )
}

# The API sends post_count as text. A plain number parses; anything else
# ("12.5K", "") becomes NA without a warning.
personalized_trend_row <- function(x) {
  tibble(
    trend_name     = as.character(x$trend_name %||% NA_character_),
    category       = as.character(x$category %||% NA_character_),
    post_count     = suppressWarnings(as.integer(x$post_count %||% NA_character_)),
    trending_since = as.character(x$trending_since %||% NA_character_)
  )
}

personalized_trends_table <- function(trends) {
  trends <- keep(trends %||% list(), is.list)
  if (length(trends) == 0) {
    return(personalized_trend_schema())
  }
  map_dfr(trends, personalized_trend_row)
}
