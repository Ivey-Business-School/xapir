## Spaces: live audio rooms on X. Three readers return a table of spaces and
## one returns the posts shared in a space. All four work with an app bearer
## token. The space table's schema, row builder and table builder live at the
## bottom of this file.

#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr keep map_dfr
#' @importFrom tibble tibble
#' @importFrom stringr str_c
NULL

# Every space field the table reads. The API sends creator_id, host_ids,
# speaker_ids and topic_ids without being asked.
default_space_fields <- function() {
  c(
    "created_at", "ended_at", "id", "is_ticketed", "lang", "participant_count",
    "scheduled_start", "started_at", "state", "subscriber_count", "title",
    "updated_at"
  )
}

#' Get Spaces by IDs or by Creator IDs
#'
#' @description
#' Retrieves up to 100 spaces by their ids via the
#' [get spaces by IDs endpoint](https://docs.x.com/x-api/spaces/get-spaces-by-ids),
#' or every space created by up to 100 users via the
#' [get spaces by creator IDs endpoint](https://docs.x.com/x-api/spaces/get-spaces-by-creator-ids).
#' Give exactly one of `space_ids` and `user_ids`. Every space returned is
#' billed, so the function says what the call can cost before it reads
#' anything. An app bearer token is enough.
#'
#' An id the API cannot find does not stop the call: the spaces it did find
#' are returned, and one warning names each id that was not.
#'
#' @param space_ids A character vector of up to 100 space ids, each a short
#'   string of letters and digits such as `"1DXxyRYNejbKM"`.
#' @param user_ids A character vector of up to 100 user ids, each a string
#'   of digits. Keep ids as text: as numbers they lose digits. Returns the
#'   spaces those users created, so one user id can give several rows.
#' @template bearer_token
#' @param space_fields \code{character}, \code{vector}; the fields to return
#'   for each space. The default asks for everything the table reads.
#' @return A tibble with one row per space: `space_id`, `title`, `state`
#'   (`"live"`, `"scheduled"` or `"ended"`), `creator_id`, `created_at`,
#'   `scheduled_start`, `started_at` and `ended_at` (POSIXct, UTC), `lang`,
#'   `is_ticketed`, `participant_count`, `subscriber_count`, `host_ids` and
#'   `speaker_ids` (list-columns, one character vector of user ids per row).
#'   When no space is found, the same columns with no rows.
#' @examples
#' \dontrun{
#' spaces <- get_spaces(space_ids = c("1DXxyRYNejbKM", "1nAKErYNqlpxL"))
#' spaces <- get_spaces(user_ids = c("783214", "2244994945"))
#' }
#' @export
get_spaces <- function(
  space_ids    = NULL,
  user_ids     = NULL,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  space_fields = default_space_fields()
) {
  check_token(bearer_token)
  if (is.null(space_ids) == is.null(user_ids)) {
    stop(
      "Give either `space_ids` or `user_ids`, not both and not neither.",
      call. = FALSE
    )
  }

  if (!is.null(space_ids)) {
    space_ids <- check_space_ids(space_ids)
    announce_cap(length(space_ids), what = "spaces", arg = "space_ids")
    req <- x_request(bearer_token) |>
      req_url_path_append("spaces") |>
      req_url_query(ids = str_c(space_ids, collapse = ","))
    what <- "space ids"
  } else {
    user_ids <- check_user_ids(user_ids)
    announce_cap(length(user_ids), what = "spaces", arg = "user_ids")
    req <- x_request(bearer_token) |>
      req_url_path_append("spaces", "by", "creator_ids") |>
      req_url_query(user_ids = str_c(user_ids, collapse = ","))
    what <- "user ids"
  }

  page <- req |>
    req_url_query(space.fields = join_fields(space_fields)) |>
    x_perform()

  warn_partial_errors(page$errors, what = what)
  spaces_table(page$data)
}

#' Search Spaces
#'
#' @description
#' Finds live or scheduled spaces whose title matches a search via the
#' [search spaces endpoint](https://docs.x.com/x-api/spaces/search-spaces).
#' Every space returned is billed, so the function says what the call can
#' cost before it reads anything. An app bearer token is enough.
#'
#' @param query One search string, matched against space titles.
#' @param state Which spaces to return: `"live"`, `"scheduled"` or `"all"`
#'   (the default).
#' @param max_results The most spaces to return, between 1 and 100. Default
#'   100. The function stops before any request if the value is outside that
#'   range.
#' @template bearer_token
#' @inheritParams get_spaces
#' @return A tibble with one row per space and the columns described in
#'   [get_spaces()]: `space_id`, `title`, `state`, `creator_id`, `created_at`,
#'   `scheduled_start`, `started_at`, `ended_at` (POSIXct, UTC), `lang`,
#'   `is_ticketed`, `participant_count`, `subscriber_count`, `host_ids` and
#'   `speaker_ids` (list-columns). When nothing matches, the same columns
#'   with no rows.
#' @examples
#' \dontrun{
#' live <- search_spaces("marketing", state = "live", max_results = 20)
#' }
#' @export
search_spaces <- function(
  query,
  state        = "all",
  max_results  = 100,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  space_fields = default_space_fields()
) {
  check_token(bearer_token)
  check_query(query)
  check_space_state(state)
  check_max_results(max_results, min = 1, max = 100, what = "spaces")
  announce_cap(max_results, what = "spaces", arg = "max_results")

  page <- x_request(bearer_token) |>
    req_url_path_append("spaces", "search") |>
    req_url_query(
      query        = query,
      state        = state,
      max_results  = as.integer(max_results),
      space.fields = join_fields(space_fields)
    ) |>
    x_perform()

  warn_partial_errors(page$errors, what = "spaces")
  spaces_table(page$data)
}

#' Get Space Posts
#'
#' @description
#' Returns the posts shared in a space via the
#' [get space posts endpoint](https://docs.x.com/x-api/spaces/get-space-posts).
#' Every post returned is billed, so the function says what the call can
#' cost before it reads anything. An app bearer token is enough.
#'
#' @param space_id The space's id, one short string of letters and digits
#'   such as `"1DXxyRYNejbKM"`.
#' @param max_results The most posts to return, between 1 and 100. Default
#'   100. The function stops before any request if the value is outside that
#'   range.
#' @template bearer_token
#' @template post_fields
#' @template user_fields
#' @template media_fields
#' @template poll_fields
#' @template place_fields
#' @template expansions
#' @return A \code{list} holding one page, in the same shape as [get_post()]
#'   returns, so the `extract_*()` functions accept it. A space with no
#'   shared posts gives a page with no `data`.
#' @examples
#' \dontrun{
#' page  <- get_space_posts("1DXxyRYNejbKM", max_results = 50)
#' posts <- extract_post(page)
#' }
#' @export
get_space_posts <- function(
  space_id,
  max_results  = 100,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  post_fields  = default_post_fields(),
  user_fields  = default_user_fields(),
  media_fields = default_media_fields(),
  poll_fields  = default_poll_fields(),
  place_fields = default_place_fields(),
  expansions   = default_expansions()
) {
  check_token(bearer_token)
  check_space_ids(space_id, max_ids = 1, arg = "space_id")
  check_max_results(max_results, min = 1, max = 100, what = "posts")
  announce_cap(max_results, arg = "max_results")

  page <- x_request(bearer_token) |>
    req_url_path_append("spaces", space_id, "tweets") |>
    req_url_query(
      max_results = as.integer(max_results),
      !!!field_query(post_fields, user_fields, media_fields, poll_fields,
                     place_fields, expansions)
    ) |>
    x_perform()

  warn_partial_errors(page$errors, what = "posts")
  list(page)
}

# Guardrails -----------------------------------------------------------------

# Space ids are short strings of letters and digits, not the long digit
# strings posts and users have. Stops before any request when an id has the
# wrong shape or there are more than the endpoint takes in one call.
check_space_ids <- function(space_ids, max_ids = 100, arg = "space_ids") {
  ok <- is.character(space_ids) && length(space_ids) >= 1 &&
    !anyNA(space_ids) && all(grepl("^[a-zA-Z0-9]{1,13}$", space_ids))
  if (!ok) {
    stop(
      "`", arg, "` must be ",
      if (max_ids == 1) "one string" else "a character vector of strings",
      " of letters and digits, such as \"1DXxyRYNejbKM\".",
      call. = FALSE
    )
  }
  if (length(space_ids) > max_ids) {
    stop(
      "`", arg, "` can hold at most ", max_ids,
      if (max_ids == 1) " id" else " ids", " per call. ",
      "Split the vector and call the function once per ", max_ids, ".",
      call. = FALSE
    )
  }
  invisible(space_ids)
}

check_space_state <- function(state) {
  choices <- c("live", "scheduled", "all")
  ok <- is.character(state) && length(state) == 1 && !is.na(state) &&
    state %in% choices
  if (!ok) {
    stop("`state` must be \"live\", \"scheduled\" or \"all\".", call. = FALSE)
  }
  invisible(state)
}

# Table ----------------------------------------------------------------------

# The 14 columns every space table has, in this order, with no rows.
space_schema <- function() {
  tibble(
    space_id          = character(0),
    title             = character(0),
    state             = character(0),
    creator_id        = character(0),
    created_at        = as.POSIXct(character(0), tz = "UTC"),
    scheduled_start   = as.POSIXct(character(0), tz = "UTC"),
    started_at        = as.POSIXct(character(0), tz = "UTC"),
    ended_at          = as.POSIXct(character(0), tz = "UTC"),
    lang              = character(0),
    is_ticketed       = logical(0),
    participant_count = integer(0),
    subscriber_count  = integer(0),
    host_ids          = list(),
    speaker_ids       = list()
  )
}

# A JSON array of ids becomes one character vector, empty when absent.
id_vector <- function(x) {
  as.character(unlist(x %||% list()))
}

# One row of the space table from one parsed space. Every field the API may
# leave out gets a typed NA, so rows always bind.
space_row <- function(x) {
  tibble(
    space_id          = as.character(x$id %||% NA_character_),
    title             = as.character(x$title %||% NA_character_),
    state             = as.character(x$state %||% NA_character_),
    creator_id        = as.character(x$creator_id %||% NA_character_),
    created_at        = parse_x_time(x$created_at),
    scheduled_start   = parse_x_time(x$scheduled_start),
    started_at        = parse_x_time(x$started_at),
    ended_at          = parse_x_time(x$ended_at),
    lang              = as.character(x$lang %||% NA_character_),
    is_ticketed       = as.logical(x$is_ticketed %||% NA),
    participant_count = as.integer(x$participant_count %||% NA_integer_),
    subscriber_count  = as.integer(x$subscriber_count %||% NA_integer_),
    host_ids          = list(id_vector(x$host_ids)),
    speaker_ids       = list(id_vector(x$speaker_ids))
  )
}

# A list of parsed spaces to one table, one row per space id. An entry with
# no id is not a space and is dropped. Returns the empty schema when nothing
# is left, and never warns.
spaces_table <- function(spaces) {
  spaces <- keep(spaces %||% list(), ~ is.list(.x) && !is.null(.x$id))
  if (length(spaces) == 0) {
    return(space_schema())
  }
  out <- map_dfr(spaces, space_row)
  out[!duplicated(out$space_id), ]
}
