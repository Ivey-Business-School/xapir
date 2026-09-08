#' Get Own Liked Posts
#'
#' @description
#' Retrieves a list of Posts liked by a specific User via the [get
#' liked posts endpoint](https://docs.x.com/x-api/posts/get-liked-posts).
#' Needs a user token, so the first call opens a browser window to sign in.
#'
#' @template username
#' @template max_results
#' @template max_posts
#' @template pagination_token
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
#' tl <- get_liked_posts(username = "XDevelopers")
#' }
#' @export
get_liked_posts <- function(
    username,
    max_results      = 100,
    max_posts        = 500,
    pagination_token = NULL,
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
    req_url_path_append("users", user_id, "liked_tweets") |>
    req_url_query(
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
