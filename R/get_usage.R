#' Get Usage
#'
#' @description
#' Returns how many posts your project has read against its monthly cap via
#' the [get usage endpoint](https://docs.x.com/x-api/usage/get-usage). This
#' is your own spend, so no cost line is printed: the pricing page does not
#' list the usage endpoints as billed.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr pluck map_chr map_int
#' @importFrom tibble tibble
#' @importFrom lubridate ymd_hms
#' @param days \code{numeric}; how many days of daily usage to return,
#'   between 1 and 90. Default 7.
#' @template bearer_token
#' @return A tibble with one row: `project_id` (character), `project_cap`
#'   (integer, posts a month), `project_usage` (integer, posts read so far
#'   this cycle) and `cap_reset_day` (integer, the day of the month the
#'   count restarts). The daily breakdown is attached as the attribute
#'   `"daily"`: a tibble with `date` (Date) and `usage` (integer), one row
#'   per day, oldest first. Read it with `attr(usage, "daily")`.
#' @examples
#' \dontrun{
#' usage <- get_usage()
#' attr(usage, "daily")
#' }
#' @export
get_usage <- function(
  days         = 7,
  bearer_token = Sys.getenv("X_BEARER_TOKEN")
) {
  check_token(bearer_token)
  check_usage_days(days)

  body <- x_request(bearer_token) |>
    req_url_path_append("usage", "tweets") |>
    req_url_query(
      days = as.integer(days),
      usage.fields = join_fields(c(
        "cap_reset_day", "daily_client_app_usage", "daily_project_usage",
        "project_cap", "project_id", "project_usage"
      ))
    ) |>
    x_perform()

  warn_partial_errors(body$errors, what = "usage")
  usage <- pluck(body, "data") %||% list()

  # The API sends the cap and usage as strings of digits.
  summary <- tibble(
    project_id    = as.character(usage$project_id %||% NA_character_),
    project_cap   = as.integer(usage$project_cap %||% NA_integer_),
    project_usage = as.integer(usage$project_usage %||% NA_integer_),
    cap_reset_day = as.integer(usage$cap_reset_day %||% NA_integer_)
  )

  entries <- pluck(usage, "daily_project_usage", "usage") %||% list()
  daily <- tibble(
    date  = as.Date(ymd_hms(
      map_chr(entries, "date", .default = NA_character_),
      tz = "UTC", quiet = TRUE
    )),
    usage = as.integer(map_chr(entries, "usage", .default = NA_character_))
  )
  daily <- daily[order(daily$date), ]

  attr(summary, "daily") <- daily
  summary
}

#' Get Usage Credits
#'
#' @description
#' Returns the dollar balance left on your pay-per-use account via the
#' [get usage credits endpoint](https://docs.x.com/x-api/usage/get-usage-credits).
#' This is your own balance, so no cost line is printed: the pricing page
#' does not list the usage endpoints as billed.
#'
#' The API reference allows either a user token or an app bearer token for
#' this endpoint. The function sends the bearer token; if the API answers
#' that it wants a user token, the call stops with that message.
#'
#' @importFrom httr2 req_url_path_append
#' @importFrom purrr pluck
#' @importFrom tibble tibble
#' @template bearer_token
#' @return A tibble with one row: `total_balance`, `prepaid_balance` and
#'   `free_balance`, each in US dollars (double). `total_balance` is what
#'   can still be spent: the prepaid balance plus unexpired free credit,
#'   never below zero.
#' @examples
#' \dontrun{
#' credits <- get_usage_credits()
#' }
#' @export
get_usage_credits <- function(bearer_token = Sys.getenv("X_BEARER_TOKEN")) {
  check_token(bearer_token)

  body <- x_request(bearer_token) |>
    req_url_path_append("usage", "credits") |>
    x_perform()

  warn_partial_errors(body$errors, what = "credits")
  credits <- pluck(body, "data") %||% list()

  tibble(
    total_balance   = as.double(credits$total_balance %||% NA_real_),
    prepaid_balance = as.double(credits$prepaid_balance %||% NA_real_),
    free_balance    = as.double(credits$free_balance %||% NA_real_)
  )
}

# The usage endpoint takes 1 to 90 days of history.
check_usage_days <- function(days) {
  ok <- is.numeric(days) && length(days) == 1 && !is.na(days) &&
    days >= 1 && days <= 90
  if (!ok) {
    stop(
      "`days` must be a number between 1 and 90.",
      call. = FALSE
    )
  }
  invisible(days)
}
