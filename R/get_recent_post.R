#' Get Recent Post
#'
#' @description
#' Returns Posts from the last 7 days that match a search query via the [recent search
#' endpoint](https://docs.x.com/x-api/posts/recent-search).
#'
#' @param query The search to be made on X. You can find ways to build specific queries according to the [X API documentation website](https://docs.x.com/x-api/posts/search/integrate/build-a-query#types)
#' @template max_results
#' @template max_posts
#' @param end_time The latest date-time from which you want to get posts.
#'   Provide the value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
#'   `iso_8601()` function will convert a string, date, or date-time object to
#'   the required format (e.g., `iso_8601("2024-10-10")`).
#' @param start_time The earliest date-time from which you want to get posts.
#' @param sort_order The order of the posts returned: 'relevancy' or 'recency'.
#' @param until_id A post ID to limit the results to posts older than the
#'   specified ID.
#' @param since_id A post ID to limit the results to posts more recent than the
#'   specified ID.
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
#' tl <- get_recent_post("Developers")
#' }
#' @export
get_recent_post <- function(
    query,
    max_results      = 100,
    max_posts        = 3200,
    end_time         = NULL,
    start_time       = NULL,
    sort_order       = "relevancy",
    until_id         = NULL,
    since_id         = NULL,
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
  check_max_results(max_results)
  check_max_posts(max_posts)
  announce_cap(max_posts)

  req <- x_request(bearer_token) |>
    req_url_path_append("tweets", "search", "recent") |>
    req_url_query(
      query      = query,
      end_time   = end_time,
      start_time = start_time,
      until_id   = until_id,
      since_id   = since_id,
      sort_order = sort_order,
      !!!field_query(post_fields, user_fields, media_fields, poll_fields,
                     place_fields, expansions)
    )

  fetch_pages(
    req,
    max_posts        = max_posts,
    max_results      = max_results,
    sleep_time       = sleep_time,
    pagination_token = pagination_token
  )
}
