#' Like Post
#'
#' @description
#' Likes a post from the signed-in account via the [like post
#' endpoint](https://docs.x.com/x-api/users/like-post). Needs a user token,
#' so the first call opens a browser window to sign in. The request is billed
#' as one interaction, so the function says what it costs before it sends
#' anything.
#'
#' @importFrom httr2 req_body_json req_method req_url_path_append
#' @param post_id The id of the post to like, as a string.
#' @return Invisibly, the `data` list the API returns, `list(liked = TRUE)`.
#'   Stops with the API's message when the like is refused.
#' @examples
#' \dontrun{
#' like_post(post_id = "20")
#' }
#' @export
like_post <- function(
  post_id
) {

  check_post_id(post_id)
  token   <- authenticate_user()
  user_id <- my_user_id(token)

  announce_request_cost("interaction")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "likes") |>
    req_method("POST") |>
    req_body_json(list(tweet_id = post_id)) |>
    x_perform()

  invisible(response$data)
}

#' Unlike Post
#'
#' @description
#' Removes the signed-in account's like from a post via the [unlike post
#' endpoint](https://docs.x.com/x-api/users/unlike-post). Needs a user token,
#' so the first call opens a browser window to sign in. The request is
#' billed, so the function says what it costs before it sends anything.
#'
#' @importFrom httr2 req_method req_url_path_append
#' @param post_id The id of the post to unlike, as a string.
#' @return Invisibly, the `data` list the API returns, `list(liked = FALSE)`.
#'   Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' unlike_post(post_id = "20")
#' }
#' @export
unlike_post <- function(
  post_id
) {

  check_post_id(post_id)
  token   <- authenticate_user()
  user_id <- my_user_id(token)

  announce_request_cost("interaction_delete")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "likes", post_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}
