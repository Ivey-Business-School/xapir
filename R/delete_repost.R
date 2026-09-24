#' Delete Repost
#'
#' @description
#' Removes the signed-in account's repost of a post via the [unrepost
#' endpoint](https://docs.x.com/x-api/posts/unrepost-post). Needs a user
#' token, so the first call opens a browser window to sign in.
#'
#' @importFrom httr2 req_method
#' @param post_id The id of the post whose repost to remove, as a string.
#' @param tweet_id Deprecated. Use `post_id`.
#' @return Invisibly, the `data` list the API returns, `list(retweeted =
#'   FALSE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' delete_repost(post_id = "20")
#' }
#' @export
delete_repost <- function(
  post_id,
  tweet_id = NULL
) {

  post_id <- use_post_id(post_id, tweet_id)
  token   <- authenticate_user()
  user_id <- my_user_id(token)

  announce_request_cost("interaction_delete")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "retweets", post_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}
