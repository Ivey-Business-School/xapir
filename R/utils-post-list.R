#' Collect the posts in a raw response
#'
#' @description
#' Every reader returns a list of pages. Each page holds the posts the
#' endpoint returned in `data`, and the posts they quote, reply to or repost
#' in `includes$tweets`. This helper flattens both into one list, `data`
#' first, so a post that appears in both keeps its `data` copy when the
#' caller de-duplicates.
#'
#' @param timeline A list of pages as returned by a reader such as
#'   [get_timeline()].
#' @param include_referenced_posts Logical. Whether to append the posts in
#'   `includes$tweets` after the posts in `data`.
#' @return A list of posts, each a named list as parsed from the API's JSON.
#' @keywords internal
#' @noRd
post_list <- function(timeline, include_referenced_posts = TRUE) {
  from_data <- timeline |>
    map(~ pluck(.x, "data")) |>
    unlist(recursive = FALSE)
  from_data <- from_data %||% list()

  if (!include_referenced_posts) {
    return(from_data)
  }

  from_includes <- timeline |>
    map(~ pluck(.x, "includes", "tweets")) |>
    unlist(recursive = FALSE)
  from_includes <- from_includes %||% list()

  c(from_data, from_includes)
}

#' Collect one block of `includes` across every page
#'
#' @param timeline A list of pages.
#' @param block The name of the block: "users", "media", "polls" or "places".
#' @return A list, possibly empty.
#' @keywords internal
#' @noRd
includes_list <- function(timeline, block) {
  out <- timeline |>
    map(~ pluck(.x, "includes", block)) |>
    unlist(recursive = FALSE)
  out %||% list()
}

#' Map user ids to usernames from a raw response
#'
#' @description
#' Reads `includes$users` on every page and returns one row per user id.
#' Used to build `post_url` from the author's handle.
#'
#' @inheritParams post_list
#' @return A tibble with `user_id` and `username`, both character.
#' @keywords internal
#' @noRd
author_lookup <- function(timeline) {
  users <- includes_list(timeline, "users")

  if (length(users) == 0) {
    return(tibble(user_id = character(0), username = character(0)))
  }

  tibble(
    user_id  = map_chr(users, ~ .x$id %||% NA_character_),
    username = map_chr(users, ~ .x$username %||% NA_character_)
  ) |>
    filter(!is.na(user_id)) |>
    distinct(user_id, .keep_all = TRUE)
}
