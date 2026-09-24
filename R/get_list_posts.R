#' Get List Posts
#'
#' @description
#' Returns the posts on a list's timeline via the
#' [get list posts endpoint](https://docs.x.com/x-api/lists/get-list-posts).
#' Every post returned is billed (US$0.005 each in September 2026), so the
#' function says what the call can cost before it reads anything, and stops
#' reading at `max_posts`.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @param list_id The list's id, as a string of digits.
#' @param max_results \code{numeric}; the number of posts per API call,
#'   between 1 and 100. The function stops before any request if the value
#'   is outside that range.
#' @template max_posts
#' @template pagination_token
#' @template sleep_time
#' @template bearer_token
#' @template post_fields
#' @template user_fields
#' @template media_fields
#' @template poll_fields
#' @template place_fields
#' @template expansions
#' @return A \code{list} of pages. Each page holds `data`, `includes` and
#'   `meta` as the API returned them. Pass it to the `extract_*()` functions.
#' @examples
#' \dontrun{
#' pages <- get_list_posts(list_id = "1146654567674912769")
#' posts <- extract_post(pages)
#' }
#' @export
get_list_posts <- function(
  list_id,
  max_results      = 100,
  max_posts        = 500,
  pagination_token = NULL,
  sleep_time       = 0,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  post_fields      = default_post_fields(),
  user_fields      = default_user_fields(),
  media_fields     = default_media_fields(),
  poll_fields      = default_poll_fields(),
  place_fields     = default_place_fields(),
  expansions       = default_expansions()
) {
  check_token(bearer_token)
  check_list_id(list_id)
  check_max_results(max_results, min = 1, max = 100)
  check_max_posts(max_posts)
  announce_cap(max_posts)

  req <- x_request(bearer_token) |>
    req_url_path_append("lists", list_id, "tweets") |>
    req_url_query(
      !!!field_query(
        post_fields, user_fields, media_fields, poll_fields,
        place_fields, expansions
      )
    )

  pages <- fetch_pages(
    req,
    max_posts        = max_posts,
    max_results      = max_results,
    sleep_time       = sleep_time,
    pagination_token = pagination_token,
    min_results      = 1
  )

  for (page in pages) {
    warn_partial_errors(page$errors, what = "posts")
  }

  pages
}
