#' Get Repost by ID
#'
#' @description
#' Retrieves a list of Posts that repost a specific Post by its ID
#' via the [get repost endpoint](https://docs.x.com/x-api/posts/get-reposts).
#'
#' @param post_id The ID of the post whose reposts you want.
#' @template max_results
#' @template bearer_token
#' @template post_fields
#' @template user_fields
#' @template media_fields
#' @template poll_fields
#' @template place_fields
#' @template expansions
#' @return A \code{list} holding one page, in the same shape as
#'   [get_timeline()] returns, so the `extract_*()` functions accept it.
#' @examples
#' \dontrun{
#' post <- get_repost("1234567890123456789")
#' }
#' @export
get_repost <- function(
  post_id,
  max_results      = 100,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  post_fields      = default_post_fields(),
  user_fields      = default_user_fields(),
  media_fields     = default_media_fields(),
  poll_fields      = default_poll_fields(),
  place_fields     = default_place_fields(),
  expansions       = default_expansions()
) {

  check_max_results(max_results)

  page <- x_request(bearer_token) |>
    req_url_path_append("tweets", post_id, "retweets") |>
    req_url_query(
      max_results = max_results,
      !!!field_query(post_fields, user_fields, media_fields, poll_fields,
                     place_fields, expansions)
    ) |>
    x_perform()

  list(page)
}
