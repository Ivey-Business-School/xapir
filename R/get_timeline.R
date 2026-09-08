#' Get User Timeline
#'
#' @description
#' Returns a list of Posts authored by the provided User ID via the [user posts timeline by user ID
#' endpoint](https://docs.x.com/x-api/posts/get-posts).
#'
#' @template username
#' @template max_results
#' @template max_posts
#' @param end_time The latest date-time from which you want to get posts.
#'   Provide the value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
#'   `iso_8601()` function will convert a string, date, or date-time object to
#'   the required format (e.g., `iso_8601("2024-10-10")`).
#' @param start_time The earliest date-time from which you want to get posts.
#' @param until_id A post ID to limit the results to posts older than the
#'   specified ID.
#' @param since_id A post ID to limit the results to posts more recent than the
#'   specified ID.
#' @template pagination_token
#' @param exclude A comma-separated list of the types of posts to exclude from
#'   the response (e.g., "retweets", "replies", or "retweets,replies").
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
#' tl <- get_timeline("XDevelopers")
#' }
#' @export
get_timeline <- function(
    username,
    max_results      = 100,
    max_posts        = 500,
    end_time         = NULL,
    start_time       = NULL,
    until_id         = NULL,
    since_id         = NULL,
    pagination_token = NULL,
    exclude          = NULL,
    sleep_time       = 90,
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

  user_id <- lookup_user_id(username, bearer_token)

  req <- x_request(bearer_token) |>
    req_url_path_append("users", user_id, "tweets") |>
    req_url_query(
      end_time   = end_time,
      start_time = start_time,
      until_id   = until_id,
      since_id   = since_id,
      exclude    = exclude,
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
