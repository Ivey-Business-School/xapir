#' Delete Post(s) on X
#'
#' @description
#' Delete one or more Posts (in the path) by ID via the [delete a post 
#' endpoint](https://docs.x.com/x-api/posts/post-delete-by-post-id).
#'
#' @importFrom httr2 request req_url_path_append req_auth_bearer_token req_perform resp_body_json
#' @importFrom purrr map_chr map_lgl map
#' @importFrom tibble tibble
#' @param post_ids The IDs of the posts you want to delete.
#' @return A tibble with post_id, deleted (TRUE/FALSE), and error (if any).
#' @examples
#' \dontrun{
#' delete_from_x(post_id = targeted_post_ids)
#' }
#' @export
delete_from_x <- function(
  post_ids
) {
  # Get cached or refreshed token
  token <- authenticate_user()

  # Ensure character vector
  post_ids <- as.character(post_ids)

  # Loop through and delete each post
  results <- lapply(post_ids, function(post_id) {
    req <- request("https://api.twitter.com/2/tweets") |>
      req_url_path_append(post_id) |>
      req_auth_bearer_token(token$access_token) |>
      req_method("DELETE")

    tryCatch({
      resp <- req_perform(req)
      json <- resp_body_json(resp)
      list(post_id = post_id, deleted = json$data$deleted %||% NA, error = NA_character_)
    }, error = function(e) {
      list(post_id = post_id, deleted = FALSE, error = e$message)
    })
  })

  # Convert to tibble
  responses <- tibble(
    post_id = map_chr(results, "post_id"),
    deleted = map_lgl(results, "deleted"),
    error   = map_chr(results, "error")
  )

  return(responses)
}
