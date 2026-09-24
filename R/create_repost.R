#' Create Repost
#'
#' @description
#' Reposts a post from the signed-in account via the [repost
#' endpoint](https://docs.x.com/x-api/posts/repost-post). Needs a user token,
#' so the first call opens a browser window to sign in.
#'
#' @importFrom httr2 req_body_json req_method
#' @param post_id The id of the post to repost, as a string.
#' @param tweet_id Deprecated. Use `post_id`.
#' @return Invisibly, the `data` list the API returns, `list(retweeted =
#'   TRUE)`. Stops with the API's message when the repost is refused.
#' @examples
#' \dontrun{
#' create_repost(post_id = "20")
#' }
#' @export
create_repost <- function(
  post_id,
  tweet_id = NULL
) {

  post_id <- use_post_id(post_id, tweet_id)
  token   <- authenticate_user()
  user_id <- my_user_id(token)

  announce_request_cost("interaction")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "retweets") |>
    req_method("POST") |>
    req_body_json(list(tweet_id = post_id)) |>
    x_perform()

  invisible(response$data)
}
