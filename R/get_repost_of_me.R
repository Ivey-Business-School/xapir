#' Get Reposts of Me
#'
#' @description
#' Retrieves a list of Posts that repost content from the authenticated user
#' via the [get reposts of me endpoint](https://docs.x.com/x-api/users/get-reposts-of-me).
#' Needs a user token, so the first call opens a browser window to sign in.
#'
#' @template max_results
#' @template post_fields
#' @template user_fields
#' @template media_fields
#' @template poll_fields
#' @template place_fields
#' @template expansions
#' @return A \code{list} holding one page, in the same shape as
#'   [get_timeline()] returns, so the `extract_*()` functions accept it. The
#'   page holds at most `max_results` posts, and that worst case is printed
#'   in posts and dollars before the request.
#' @examples
#' \dontrun{
#' post <- get_repost_of_me()
#' }
#' @export
get_repost_of_me <- function(
  max_results      = 100,
  post_fields      = default_post_fields(),
  user_fields      = default_user_fields(),
  media_fields     = default_media_fields(),
  poll_fields      = default_poll_fields(),
  place_fields     = default_place_fields(),
  expansions       = default_expansions()
) {

  check_max_results(max_results)
  announce_cap(max_results, arg = "max_results")

  token <- authenticate_user()

  page <- x_request(token$access_token) |>
    req_url_path_append("users", "reposts_of_me") |>
    req_url_query(
      max_results = max_results,
      !!!field_query(post_fields, user_fields, media_fields, poll_fields,
                     place_fields, expansions)
    ) |>
    x_perform()

  list(page)
}
