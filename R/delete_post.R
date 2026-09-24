#' Delete Posts
#'
#' @description
#' Deletes one or more of the signed-in account's posts by id via the [delete
#' a post endpoint](https://docs.x.com/x-api/posts/post-delete-by-post-id).
#' Needs a user token, so the first call opens a browser window to sign in.
#'
#' X caps how many posts an account may delete in a 15-minute window, and
#' the cap is lowest on the Free tier. The ids are therefore deleted in
#' batches of `batch_size`, with a pause of `sleep_time` seconds between
#' batches. The defaults, 5 posts then 15 minutes, stay inside the cap on
#' every tier; raise `batch_size` on a paid tier (up to 50) to go faster.
#'
#' A post that cannot be deleted does not stop the others: its row records
#' the API's message and the function carries on.
#'
#' @importFrom httr2 req_method
#' @param post_ids A character vector of the ids of the posts to delete.
#' @param sleep_time Seconds to pause between batches.
#' @param batch_size Number of posts to delete before pausing.
#' @return A tibble with one row per id in `post_ids`: `post_id` (the id),
#'   `deleted` (`TRUE` when the API confirmed the deletion) and `error` (the
#'   error message, or `NA` when the post was deleted).
#' @examples
#' \dontrun{
#' delete_post(post_ids = c("1234567890123456789", "1234567890123456790"))
#' }
#' @export
delete_post <- function(
  post_ids,
  sleep_time = 900,
  batch_size = 5
) {

  post_ids <- as.character(post_ids)
  if (length(post_ids) == 0) {
    return(tibble(post_id = character(), deleted = logical(),
                  error = character()))
  }
  if (!is.numeric(batch_size) || length(batch_size) != 1 || batch_size < 1) {
    stop("`batch_size` must be a number of 1 or more.", call. = FALSE)
  }

  batches <- split(post_ids, ceiling(seq_along(post_ids) / batch_size))
  rows    <- list()

  for (i in seq_along(batches)) {
    # Fetched per batch: a pause can outlive the access token, and the cache
    # refreshes it without a browser.
    token <- authenticate_user()

    for (post_id in batches[[i]]) {
      rows[[length(rows) + 1]] <- delete_one_post(post_id, token)
    }

    if (i < length(batches)) {
      message(sprintf(
        "Deleted batch %d of %d. Pausing %d seconds before the next one...",
        i, length(batches), as.integer(sleep_time)
      ))
      Sys.sleep(sleep_time)
    }
  }

  bind_rows(rows)
}

# One row for one id, whatever happens.
delete_one_post <- function(post_id, token) {
  tryCatch(
    {
      response <- x_request(token$access_token) |>
        req_url_path_append("tweets", post_id) |>
        req_method("DELETE") |>
        x_perform()
      tibble(
        post_id = post_id,
        deleted = isTRUE(response$data$deleted),
        error   = NA_character_
      )
    },
    error = function(e) {
      tibble(
        post_id = post_id,
        deleted = FALSE,
        error   = conditionMessage(e)
      )
    }
  )
}
