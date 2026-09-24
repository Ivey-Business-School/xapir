## News: the stories X's Grok assembles from posts on the platform. One
## reader searches them and one looks a story up by id. Both work with an
## app bearer token. The news table's schema, row builder and table builder
## live at the bottom of this file.

#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr keep map_chr map_dfr
#' @importFrom tibble tibble
NULL

# Every news field the table reads. `cluster_posts_results` (the ids of the
# posts a story was built from) is left out: it is a list of ids no column
# reads, and can be passed through `news_fields`.
default_news_fields <- function() {
  c(
    "category", "contexts", "disclaimer", "hook", "id", "keywords", "name",
    "summary", "updated_at"
  )
}

#' Search News
#'
#' @description
#' Finds news stories breaking on X that match a search via the
#' [search news endpoint](https://docs.x.com/x-api/news/search-news).
#' Each story is a summary Grok wrote from posts on X, with a headline, a
#' hook and the entities it mentions. An app bearer token is enough.
#'
#' News is not on the X API pricing page as of 24 September 2026, so this
#' function prints no cost line. Check
#' <https://docs.x.com/x-api/getting-started/pricing> before pulling a lot.
#'
#' @param query One search string, such as `"electric vehicles"`.
#' @param max_results The most stories to return, between 1 and 100.
#'   Default 20. The function stops before any request if the value is
#'   outside that range.
#' @param max_age_hours Only stories updated within this many hours, between
#'   1 and 720 (30 days). Default 24.
#' @template bearer_token
#' @param news_fields \code{character}, \code{vector}; the fields to return
#'   for each story. The default asks for everything the table reads.
#' @return A tibble with one row per story: `news_id`, `name` (the
#'   headline), `hook`, `summary`, `category`, `disclaimer`, `keywords`
#'   (list-column, one character vector per row), `contexts` (list-column,
#'   one character vector per row of every entity, topic, ticker and team
#'   the API tagged the story with, flattened from its nested groups) and
#'   `updated_at` (POSIXct, UTC). When nothing matches, the same columns
#'   with no rows.
#' @examples
#' \dontrun{
#' news <- search_news("electric vehicles", max_results = 10, max_age_hours = 48)
#' }
#' @export
search_news <- function(
  query,
  max_results   = 20,
  max_age_hours = 24,
  bearer_token  = Sys.getenv("X_BEARER_TOKEN"),
  news_fields   = default_news_fields()
) {
  check_token(bearer_token)
  check_query(query)
  check_max_results(max_results, min = 1, max = 100, what = "news stories")
  check_max_age_hours(max_age_hours)

  page <- x_request(bearer_token) |>
    req_url_path_append("news", "search") |>
    req_url_query(
      query         = query,
      max_results   = as.integer(max_results),
      max_age_hours = as.integer(max_age_hours),
      news.fields   = join_fields(news_fields)
    ) |>
    x_perform()

  warn_partial_errors(page$errors, what = "news stories")
  news_table(page$data)
}

#' Get News Story by ID
#'
#' @description
#' Retrieves one news story by its id via the
#' [get news story by ID endpoint](https://docs.x.com/x-api/news/get-news-stories-by-id).
#' An app bearer token is enough. An id the API cannot find stops the call
#' with the API's reason.
#'
#' News is not on the X API pricing page as of 24 September 2026, so this
#' function prints no cost line.
#'
#' @param news_id The story's id, as a string of digits. Keep ids as text:
#'   as numbers they lose digits.
#' @inheritParams search_news
#' @return A tibble with one row and the columns described in
#'   [search_news()]: `news_id`, `name`, `hook`, `summary`, `category`,
#'   `disclaimer`, `keywords` and `contexts` (list-columns) and `updated_at`
#'   (POSIXct, UTC).
#' @examples
#' \dontrun{
#' story <- get_news("1989418137272422538")
#' }
#' @export
get_news <- function(
  news_id,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  news_fields  = default_news_fields()
) {
  check_token(bearer_token)
  check_news_id(news_id)

  page <- x_request(bearer_token) |>
    req_url_path_append("news", news_id) |>
    req_url_query(news.fields = join_fields(news_fields)) |>
    x_perform()

  # A story that does not exist comes back as a 200 with `errors` and no
  # `data`, the same way an unknown list does.
  if (is.null(page$data)) {
    reason <- map_chr(page$errors %||% list(), ~ .x$detail %||% .x$title %||% "")
    stop(
      "No news story found for news_id \"", news_id, "\". ",
      paste(reason, collapse = " "),
      call. = FALSE
    )
  }
  warn_partial_errors(page$errors, what = "news stories")

  # This endpoint returns one story object in `data`, not a list of them.
  news_table(list(page$data))
}

# Guardrails -----------------------------------------------------------------

check_news_id <- function(news_id) {
  ok <- is.character(news_id) && length(news_id) == 1 && !is.na(news_id) &&
    grepl("^[0-9]+$", news_id)
  if (!ok) {
    stop(
      "`news_id` must be one string of digits, such as ",
      "\"1989418137272422538\". Keep ids as text: as numbers they lose digits.",
      call. = FALSE
    )
  }
  invisible(news_id)
}

check_max_age_hours <- function(max_age_hours) {
  ok <- is.numeric(max_age_hours) && length(max_age_hours) == 1 &&
    !is.na(max_age_hours) && max_age_hours >= 1 && max_age_hours <= 720
  if (!ok) {
    stop(
      "`max_age_hours` must be a number between 1 and 720 (30 days).",
      call. = FALSE
    )
  }
  invisible(as.integer(max_age_hours))
}

# Table ----------------------------------------------------------------------

# The 9 columns every news table has, in this order, with no rows.
news_schema <- function() {
  tibble(
    news_id    = character(0),
    name       = character(0),
    hook       = character(0),
    summary    = character(0),
    category   = character(0),
    disclaimer = character(0),
    keywords   = list(),
    contexts   = list(),
    updated_at = as.POSIXct(character(0), tz = "UTC")
  )
}

# Every string inside a nested JSON value, in document order, as one
# character vector. `keywords` is a plain array of strings; `contexts` is an
# object of groups (entities, topics, finance, sports), each holding arrays
# of strings, and the group names are dropped so the values read as one
# list of tags.
flatten_strings <- function(x) {
  out <- unlist(x %||% list(), use.names = FALSE)
  if (length(out) == 0) {
    return(character(0))
  }
  out <- as.character(out)
  unique(out[!is.na(out) & nzchar(out)])
}

# One row of the news table from one parsed story. Every field the API may
# leave out gets a typed NA, so rows always bind. The live API sends the
# update time as `last_updated_at_ms` where the spec says `updated_at`; both
# are read.
news_row <- function(x) {
  tibble(
    news_id    = as.character(x$id %||% NA_character_),
    name       = as.character(x$name %||% NA_character_),
    hook       = as.character(x$hook %||% NA_character_),
    summary    = as.character(x$summary %||% NA_character_),
    category   = as.character(x$category %||% NA_character_),
    disclaimer = as.character(x$disclaimer %||% NA_character_),
    keywords   = list(flatten_strings(x$keywords)),
    contexts   = list(flatten_strings(x$contexts)),
    updated_at = parse_x_time(x$updated_at %||% x$last_updated_at_ms)
  )
}

# A list of parsed stories to one table, one row per story id. An entry
# with no id is not a story and is dropped. Returns the empty schema when
# nothing is left, and never warns.
news_table <- function(stories) {
  stories <- keep(stories %||% list(), ~ is.list(.x) && !is.null(.x$id))
  if (length(stories) == 0) {
    return(news_schema())
  }
  out <- map_dfr(stories, news_row)
  out[!duplicated(out$news_id), ]
}
