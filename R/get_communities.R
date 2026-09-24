## Communities: topic groups on X. One reader looks a community up by id
## with an app bearer token; the other searches communities and needs the
## signed-in user's token. The community table's schema, row builder and
## table builder live at the bottom of this file.

#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr keep map_chr map_dfr
#' @importFrom tibble tibble
NULL

# Every community field the table reads.
default_community_fields <- function() {
  c(
    "access", "created_at", "description", "id", "join_policy",
    "member_count", "name"
  )
}

#' Get Community by ID
#'
#' @description
#' Retrieves the details of one community by its id via the
#' [get community by ID endpoint](https://docs.x.com/x-api/communities/get-community-by-id).
#' Every community returned is billed, so the function says what the call
#' can cost before it reads anything. An app bearer token is enough. An id
#' the API cannot find stops the call with the API's reason.
#'
#' @param community_id The community's id, as a string of digits. Keep ids
#'   as text: as numbers they lose digits.
#' @template bearer_token
#' @param community_fields \code{character}, \code{vector}; the fields to
#'   return for the community. The default asks for everything the table
#'   reads.
#' @return A tibble with one row: `community_id`, `name`, `description`,
#'   `access` (`"Public"` or `"Closed"`), `join_policy` (`"Open"`,
#'   `"RestrictedJoinRequestsDisabled"`, ...), `member_count` and
#'   `created_at` (POSIXct, UTC).
#' @examples
#' \dontrun{
#' community <- get_community("1493446837214187523")
#' }
#' @export
get_community <- function(
  community_id,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  community_fields = default_community_fields()
) {
  check_token(bearer_token)
  check_community_id(community_id)
  announce_cap(1, what = "communities", arg = "community_id")

  page <- x_request(bearer_token) |>
    req_url_path_append("communities", community_id) |>
    req_url_query(community.fields = join_fields(community_fields)) |>
    x_perform()

  # A community that does not exist comes back as a 200 with `errors` and
  # no `data`, the same way an unknown list does.
  if (is.null(page$data)) {
    reason <- map_chr(page$errors %||% list(), ~ .x$detail %||% .x$title %||% "")
    stop(
      "No community found for community_id \"", community_id, "\". ",
      paste(reason, collapse = " "),
      call. = FALSE
    )
  }
  warn_partial_errors(page$errors, what = "communities")

  # This endpoint returns one community object in `data`, not a list of them.
  communities_table(list(page$data))
}

#' Search Communities
#'
#' @description
#' Finds communities whose name or description matches a search via the
#' [search communities endpoint](https://docs.x.com/x-api/communities/search-communities).
#' This endpoint needs the signed-in user's token, so the function calls
#' [authenticate_user()] (a browser window opens the first time). Every
#' community returned is billed, so the function says what the call can
#' cost before it reads anything.
#'
#' @param query One search string, matched against community names and
#'   descriptions.
#' @param max_results The most communities to return, between 10 and 100.
#'   Default 100. The function stops before any request if the value is
#'   outside that range.
#' @inheritParams get_community
#' @return A tibble with one row per community and the columns described in
#'   [get_community()]: `community_id`, `name`, `description`, `access`,
#'   `join_policy`, `member_count` and `created_at` (POSIXct, UTC). When
#'   nothing matches, the same columns with no rows.
#' @examples
#' \dontrun{
#' communities <- search_communities("marketing", max_results = 20)
#' }
#' @export
search_communities <- function(
  query,
  max_results      = 100,
  community_fields = default_community_fields()
) {
  check_query(query)
  check_max_results(max_results, min = 10, max = 100, what = "communities")
  announce_cap(max_results, what = "communities", arg = "max_results")

  token <- authenticate_user()

  page <- x_request(token$access_token) |>
    req_url_path_append("communities", "search") |>
    req_url_query(
      query            = query,
      max_results      = as.integer(max_results),
      community.fields = join_fields(community_fields)
    ) |>
    x_perform()

  warn_partial_errors(page$errors, what = "communities")
  communities_table(page$data)
}

# Guardrails -----------------------------------------------------------------

# A community id, one string of digits.
check_community_id <- function(community_id) {
  ok <- is.character(community_id) && length(community_id) == 1 &&
    !is.na(community_id) && grepl("^[0-9]+$", community_id)
  if (!ok) {
    stop(
      "`community_id` must be one string of digits, such as ",
      "\"1493446837214187523\". Keep ids as text: as numbers they lose digits.",
      call. = FALSE
    )
  }
  invisible(community_id)
}

# Table ----------------------------------------------------------------------

# The 7 columns every community table has, in this order, with no rows.
community_schema <- function() {
  tibble(
    community_id = character(0),
    name         = character(0),
    description  = character(0),
    access       = character(0),
    join_policy  = character(0),
    member_count = integer(0),
    created_at   = as.POSIXct(character(0), tz = "UTC")
  )
}

# One row of the community table from one parsed community. Every field the
# API may leave out gets a typed NA, so rows always bind.
community_row <- function(x) {
  tibble(
    community_id = as.character(x$id %||% NA_character_),
    name         = as.character(x$name %||% NA_character_),
    description  = as.character(x$description %||% NA_character_),
    access       = as.character(x$access %||% NA_character_),
    join_policy  = as.character(x$join_policy %||% NA_character_),
    member_count = as.integer(x$member_count %||% NA_integer_),
    created_at   = parse_x_time(x$created_at)
  )
}

# A list of parsed communities to one table, one row per community id. An
# entry with no id is not a community and is dropped. Returns the empty
# schema when nothing is left, and never warns.
communities_table <- function(communities) {
  communities <- keep(communities %||% list(), ~ is.list(.x) && !is.null(.x$id))
  if (length(communities) == 0) {
    return(community_schema())
  }
  out <- map_dfr(communities, community_row)
  out[!duplicated(out$community_id), ]
}
