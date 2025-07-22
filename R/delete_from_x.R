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

  all_results <- list()  # To store results from all deletions

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    # Map over post_ids in the current group
    group_results <- map(group, function(post_id) {
      req <- request("https://api.twitter.com/2/tweets") |>
        req_url_path_append(post_id) |>
        req_auth_bearer_token(token$access_token) |>
        req_method("DELETE")

    tryCatch(
        {
          resp <- req_perform(req)
          json <- resp_body_json(resp)
          list(
            post_id = post_id,
            deleted = json$data$deleted %||% NA,
            error = NA_character_
          )
        },
        error = function(e) {
          if (grepl("401", e$message)) {
            message(sprintf("Token expired or unauthorized while deleting post %s. Refreshing token...", post_id))
            token <- authenticate_user() 
            Sys.sleep(5)
          } else {
            list(post_id = post_id, deleted = FALSE, error = e$message)}
        }
      )
    })

    # Store results for this group
    all_results[[i]] <- group_results

    # Sleep 15 minutes between groups (skip after the last group)
    if (i < length(groups)) {
      message(paste("Sleeping after group", i, "..."))
      Sys.sleep(sleep_time)  # 15 * 60 = 900 seconds
    }
  }

  all_results_df <- bind_rows(all_results)

  return(all_results_df)
}
