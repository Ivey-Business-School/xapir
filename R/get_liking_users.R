#' Get Liking Users
#'
#' @description
#' Retrieves a list of Users who liked a specific Post by its ID via the [get
#' liking users endpoint](https://docs.x.com/x-api/posts/get-liking-users).
#' Needs a user token, so the first call opens a browser window to sign in.
#'
#' @param post_id The ID of the Post whose liking Users are to be retrieved.
#' @template max_results
#' @param max_users The most users to read in this call. Users are billed per
#'   user returned.
#' @template pagination_token
#' @template sleep_time
#' @template user_fields
#' @return A \code{list} of pages. Each page holds `data` and `meta` as the
#'   API returned them.
#' @examples
#' \dontrun{
#' users <- get_liking_users(post_id = "1234567890")
#' }
#' @export
get_liking_users <- function(
    post_id,
    max_results      = 100,
    max_users        = 500,
    pagination_token = NULL,
    sleep_time       = 90,
    user_fields      = default_user_fields()
) {

  check_max_results(max_results)
  check_max_posts(max_users)

  token <- authenticate_user()

  req <- x_request(token$access_token) |>
    req_url_path_append("tweets", post_id, "liking_users") |>
    req_url_query(user.fields = join_fields(user_fields))

  fetch_pages(
    req,
    max_posts        = max_users,
    max_results      = max_results,
    sleep_time       = sleep_time,
    pagination_token = pagination_token,
    what             = "users"
  )
}
