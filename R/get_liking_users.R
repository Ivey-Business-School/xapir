#' Get Liking Users
#'
#' @description
#' Retrieves a list of Users who liked a specific Post by its ID via the [get
#' liking users endpoint](https://docs.x.com/x-api/posts/get-liking-users).
#' Needs a user token, so the first call opens a browser window to sign in.
#'
#' @param post_id The ID of the Post whose liking Users are to be retrieved.
#' @template max_results
#' @param max_users The most users to read in this call. The API bills every
#'   user it returns (US$0.010 each in September 2026), so the function prints
#'   the cap in users and dollars before its first request and the total it
#'   read after the last page. Must be a finite number of 1 or more. Set
#'   `options(xapir.price_per_user = <dollars>)` when the price changes.
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
    sleep_time       = 0,
    user_fields      = default_user_fields()
) {

  check_post_ids(post_id, max_ids = 1, arg = "post_id")
  check_max_results(max_results)
  check_max_posts(max_users, arg = "max_users")
  announce_cap(max_users, what = "likes")

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
    what             = "likes"
  )
}
