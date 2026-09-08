#' Get Post by IDs
#'
#' @description
#' Returns a variety of information about the Post specified by the requested ID
#' via the [get posts by IDs endpoint](https://docs.x.com/x-api/posts/get-posts-by-ids).
#'
#' @param post_ids The IDs of the posts to retrieve, as a character vector of
#'   up to 100 ids.
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
#' post <- get_post(c("1234567890123456789"))
#' }
#' @export
get_post <- function(
  post_ids,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  post_fields      = default_post_fields(),
  user_fields      = default_user_fields(),
  media_fields     = default_media_fields(),
  poll_fields      = default_poll_fields(),
  place_fields     = default_place_fields(),
  expansions       = default_expansions()
) {

  if (length(post_ids) > 100) {
    stop("`post_ids` can hold at most 100 ids per call.", call. = FALSE)
  }

  page <- x_request(bearer_token) |>
    req_url_path_append("tweets") |>
    req_url_query(
      ids = str_c(as.character(post_ids), collapse = ","),
      !!!field_query(post_fields, user_fields, media_fields, poll_fields,
                     place_fields, expansions)
    ) |>
    x_perform()

  list(page)
}
