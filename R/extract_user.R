#' Extract User Data from Timeline
#'
#' @description
#' Reads the users that a timeline's pages carry in `includes$users` (the
#' authors of the posts, and the accounts they mention, reply to or quote)
#' and returns one row per user. `created_at` is in UTC.
#' `is_identity_verified` says whether X has checked the account holder's
#' identity document; `url` is the profile link as the API returns it
#' (a t.co address), and `link_in_bio` is its display form.
#'
#' @param timeline A list of pages as returned by a reader such as
#'   [get_timeline()].
#' @return A tibble with one row per user id and 18 columns: `created_at`
#'   (POSIXct, UTC), `username`, `name`, `description`, `followers_count`,
#'   `following_count`, `post_count`, `listed_count`, `like_count`,
#'   `protected`, `verified`, `verified_type`, `is_identity_verified`,
#'   `location`, `profile_image_url`, `link_in_bio`, `url` and `user_id`.
#'   A timeline with no users gives the same columns and no rows.
#' @importFrom purrr map pluck
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username    = "XDevelopers",
#'   max_results = 100,
#'   start_time  = iso_8601(Sys.Date() - 7)
#' )
#' user <- extract_user(timeline)
#' }
#' @export
extract_user <- function(timeline) {
  pages <- timeline %||% list()
  users <- pages |>
    map(~ pluck(.x, "includes", "users")) |>
    unlist(recursive = FALSE)

  users_table(users)
}
