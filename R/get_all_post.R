#' Get All Posts
#'
#' @description
#' Returns posts from the full archive, back to the first post in 2006, that
#' match a search query via the [full-archive search
#' endpoint](https://docs.x.com/x-api/posts/search-all-posts). The endpoint
#' needs pay-per-use or Enterprise access; on a tier without it the call
#' stops with the API's own message.
#'
#' Every post returned is billed (US$0.005 each in September 2026), so the
#' function says what the call can cost before it reads anything, and stops
#' reading at `max_posts`. Size a query with [get_all_post_count()] first: a
#' count is billed once, however many posts it covers.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @param query The search to be made on X. You can find ways to build
#'   specific queries according to the [X API documentation
#'   website](https://docs.x.com/x-api/posts/search/integrate/build-a-query#types)
#' @param max_results \code{numeric}; the number of posts per API call,
#'   between 10 and 500. The function stops before any request if the value
#'   is outside that range.
#' @template max_posts
#' @param start_time The earliest date-time from which you want to get posts.
#'   Provide the value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
#'   `iso_8601()` function will convert a string, date, or date-time object to
#'   the required format (e.g., `iso_8601("2024-10-10")`).
#' @param end_time The latest date-time from which you want to get posts.
#' @param since_id A post ID to limit the results to posts more recent than the
#'   specified ID.
#' @param until_id A post ID to limit the results to posts older than the
#'   specified ID.
#' @param sort_order The order of the posts returned: 'relevancy' or 'recency'.
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
#' pages <- get_all_post(
#'   "#SuperBowl lang:en",
#'   start_time = iso_8601("2020-02-01"),
#'   end_time = iso_8601("2020-02-03"),
#'   max_posts = 1000
#' )
#' posts <- extract_post(pages)
#' }
#' @export
get_all_post <- function(
  query,
  max_results      = 100,
  max_posts        = 500,
  start_time       = NULL,
  end_time         = NULL,
  since_id         = NULL,
  until_id         = NULL,
  sort_order       = "relevancy",
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
  check_query(query)
  check_max_results(max_results, min = 10, max = 500)
  check_max_posts(max_posts)
  announce_cap(max_posts)

  req <- x_request(bearer_token) |>
    req_url_path_append("tweets", "search", "all") |>
    req_url_query(
      query      = query,
      start_time = start_time,
      end_time   = end_time,
      since_id   = since_id,
      until_id   = until_id,
      sort_order = sort_order,
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
    pagination_token = pagination_token
  )

  for (page in pages) {
    warn_partial_errors(page$errors, what = "posts")
  }

  pages
}
