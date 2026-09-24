#' Delete Bookmark
#'
#' @description
#' Removes a post from the signed-in account's bookmarks via the [delete
#' bookmark endpoint](https://docs.x.com/x-api/bookmarks/delete-bookmark).
#' Needs a user token, so the first call opens a browser window to sign in.
#'
#' @importFrom httr2 req_method
#' @param post_id The id of the post to remove from the bookmarks, as a
#'   string.
#' @param username Deprecated and ignored. Bookmarks always belong to the
#'   account that signed in.
#' @param tweet_id Deprecated. Use `post_id`.
#' @return Invisibly, the `data` list the API returns, `list(bookmarked =
#'   FALSE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' delete_bookmark(post_id = "1234567890123456789")
#' }
#' @export
delete_bookmark <- function(
  post_id,
  username = NULL,
  tweet_id = NULL
) {

  warn_username_ignored(username, "delete_bookmark")
  post_id <- use_post_id(post_id, tweet_id)
  token   <- authenticate_user()
  user_id <- my_user_id(token)

  announce_request_cost("bookmark")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "bookmarks", post_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}
