#' Retrieve Quote Posts for a Given Post
#'
#' @description
#' Returns a variety of information about each Post that quotes the Post specified by the requested ID
#' via the [quote tweets endpoint](https://docs.x.com/x-api/posts/retrieve-posts-that-quote-a-post).
#'
#' @param post_id The ID of the post whose quote posts you want.
#' @template max_results
#' @param exclude A comma-separated list of the types of posts to exclude from
#'   the response (e.g., "retweets", "replies", or "retweets,replies").
#' @template pagination_token
#' @template post_fields
#' @template user_fields
#' @template media_fields
#' @template poll_fields
#' @template place_fields
#' @template expansions
#' @template bearer_token
#' @return A \code{list} holding one page, in the same shape as
#'   [get_timeline()] returns, so the `extract_*()` functions accept it.
#' @examples
#' \dontrun{
#' get_quote_post(post_id = "20", max_results = 100)
#' }
#' @export
get_quote_post <- function(
  post_id,
  max_results      = 100,
  exclude          = NULL,
  pagination_token = NULL,
  post_fields      = default_post_fields(),
  user_fields      = default_user_fields(),
  media_fields     = default_media_fields(),
  poll_fields      = default_poll_fields(),
  place_fields     = default_place_fields(),
  expansions       = default_expansions(),
  bearer_token     = Sys.getenv("X_BEARER_TOKEN")
) {

  check_max_results(max_results)

  page <- x_request(bearer_token) |>
    req_url_path_append("tweets", post_id, "quote_tweets") |>
    req_url_query(
      max_results      = max_results,
      exclude          = exclude,
      pagination_token = pagination_token,
      !!!field_query(post_fields, user_fields, media_fields, poll_fields,
                     place_fields, expansions)
    ) |>
    x_perform()

  list(page)
}
