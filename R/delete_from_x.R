#' Delete Post(s) on X
#'
#' @description
#' Delete one or more Posts (in the path) by ID via the [delete a post
#' endpoint](https://docs.x.com/x-api/posts/post-delete-by-post-id).
#'
#' @importFrom httr2 request req_url_path_append req_auth_bearer_token req_perform resp_body_json req_method
#' @importFrom purrr map_chr map_lgl
#' @importFrom tibble tibble
#' @param post_ids A character vector of post IDs that are to be deleted from your X account
#' @return A tibble containing the requested post IDs to delete, whether they were deleted successfully, and any error messages
#' @examples
#' \dontrun{
#' delete_from_x(post_ids =  c("post_id1", "post_id2", "post_id3"))
#' }
#' @export
delete_from_x <- function(
  post_ids,
  sleep_time = 900
) {
  # Get cached or refreshed token
  token <- authenticate_user()

  # Ensure character vector
  post_ids <- as.character(post_ids)

  # Split post_ids into groups of 5
  n <- length(post_ids)
  groups <- split(post_ids, rep(1:ceiling(n/5), each = 5, length.out = n))

  results <- list()

  # Loop through and delete each post
  for (i in seq_along(groups)) {

    results <- lapply(
      groups[[i]],
      function(post_id) {

        group_results <- request("https://api.twitter.com/2/tweets") |>
          req_url_path_append(post_id) |>
          req_auth_bearer_token(token$access_token) |>
          req_method("DELETE")

        tryCatch(
          {
            resp <- req_perform(req)
            json <- resp_body_json(resp)
            list(post_id = post_id, deleted = json$data$deleted %||% NA, error = NA_character_)
          },
          error = function(e) {
            list(post_id = post_id, deleted = FALSE, error = e$message)
          }
        )
      }
    )

    results <- c(results, group_results)

    # Sleep between groups, but not after the last one
    if (i < length(groups)) {
      Sys.sleep(sleep_time)
    }
  }


  # Convert to tibble
  responses <- tibble(
    post_id = map_chr(results, "post_id"),
    deleted = map_lgl(results, "deleted"),
    error   = map_chr(results, "error")
  )

  return(responses)
}
