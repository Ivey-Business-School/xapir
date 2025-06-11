#' Delete a Post on X
#'
#' @description
#' Delete specified Post (in the path) by ID via the [delete a post 
#' endpoint](https://docs.x.com/x-api/posts/post-delete-by-post-id).
#'
#' @importFrom httr2 request req_url_path_append req_auth_bearer_token req_perform resp_body_json
#' @param post_id The ID of the post you want to delete.
#' @examples
#' \dontrun{
#' delete_from_x(post_id = targeted_post_id)
#' }
#' @export
delete_from_x <- function(
    post_id
) {

  # Get cached or refreshed token
  token <- authenticate_user()

  # Construct request
  req <- request("https://api.twitter.com/2/tweets") |>
    req_url_path_append(post_id) |>
    req_auth_bearer_token(token$access_token) |>
    req_method("DELETE")

  # Perform request
  resp <- req_perform(req)
  result <- resp_body_json(resp)
}
