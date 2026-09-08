#' Get User Account Timeline
#'
#' @description
#' Retrieves a reverse chronological list of Posts in the authenticated User's
#' home timeline via the [reverse chronological timeline
#' endpoint](https://docs.x.com/x-api/posts/get-posts). Needs a user token,
#' so the first call opens a browser window to sign in.
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
#' tl <- get_account_timeline("XDevelopers")
#' }
#' @export
get_account_timeline <- function(
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
    post_fields      = default_post_fields(),
    user_fields      = default_user_fields(),
    media_fields     = default_media_fields(),
    poll_fields      = default_poll_fields(),
    place_fields     = default_place_fields(),
    expansions       = default_expansions()
) {

  check_max_results(max_results)
  check_max_posts(max_posts)
  announce_cap(max_posts)

  token   <- authenticate_user()
  user_id <- lookup_user_id(username, token$access_token)

  req <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "timelines", "reverse_chronological") |>
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
