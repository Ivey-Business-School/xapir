#' Hide Reply
#'
#' @description
#' Hides, or unhides, a reply to one of the signed-in account's posts via the
#' [hide reply endpoint](https://docs.x.com/x-api/posts/hide-reply). Needs a
#' user token, so the first call opens a browser window to sign in.
#'
#' @importFrom httr2 req_body_json req_method
#' @param reply_id The id of the reply, as a string. It must be a reply to a
#'   post by the account that signed in.
#' @param hidden `TRUE` (the default) hides the reply, `FALSE` shows it again.
#' @return Invisibly, the `data` list the API returns, `list(hidden = TRUE)`
#'   or `list(hidden = FALSE)`. Stops with the API's message when the request
#'   is refused.
#' @examples
#' \dontrun{
#' # Hide a reply
#' hide_reply(reply_id = "1234567890123456789")
#'
#' # Show it again
#' hide_reply(reply_id = "1234567890123456789", hidden = FALSE)
#' }
#' @export
hide_reply <- function(
  reply_id,
  hidden = TRUE
) {

  check_post_id(reply_id, arg = "reply_id")
  if (!is.logical(hidden) || length(hidden) != 1 || is.na(hidden)) {
    stop("`hidden` must be TRUE or FALSE.", call. = FALSE)
  }

  token <- authenticate_user()

  response <- x_request(token$access_token) |>
    req_url_path_append("tweets", reply_id, "hidden") |>
    req_method("PUT") |>
    req_body_json(list(hidden = hidden)) |>
    x_perform()

  invisible(response$data)
}
