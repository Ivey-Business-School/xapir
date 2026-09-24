#' Create Post
#'
#' @description
#' Publishes a post from the signed-in account via the [create a post
#' endpoint](https://docs.x.com/x-api/posts/creation-of-a-post). Needs a user
#' token, so the first call opens a browser window to sign in.
#'
#' @importFrom httr2 req_body_json req_method
#' @param text The text of the post. The maximum length depends on the
#'   account's tier (280 characters on a standard account, longer on
#'   Premium). The API refuses text that is too long and the error says so.
#' @param for_super_followers_only `TRUE` to show the post only to super
#'   followers.
#' @param geo A list with a `place_id`, to attach a place to the post.
#' @param media A list with `media_ids` (and optionally `tagged_user_ids`),
#'   to attach media already uploaded to X. Cannot be combined with `poll`.
#' @param nullcast `TRUE` for a promoted-only post that does not appear in
#'   the public timeline.
#' @param poll A list with `options` and `duration_minutes`, to attach a
#'   poll. Cannot be combined with `media`.
#' @param reply A list with `in_reply_to_tweet_id` (and optionally
#'   `exclude_reply_user_ids`), to post as a reply.
#' @param reply_settings Who can reply: `"following"`, `"mentionedUsers"`
#'   or `"subscribers"`. Leave `NULL` to let everyone reply.
#' @return Invisibly, the `data` list the API returns, with the new post's
#'   `id` and `text`. Stops with the API's message when the post is refused.
#' @examples
#' \dontrun{
#' new_post <- create_post(text = "Hello, world!")
#' new_post$id
#'
#' create_post(
#'   text  = "Which one?",
#'   poll  = list(options = c("This", "That"), duration_minutes = 60)
#' )
#' }
#' @export
create_post <- function(
  text,
  for_super_followers_only = FALSE,
  geo = NULL,
  media = NULL,
  nullcast = FALSE,
  poll = NULL,
  reply = NULL,
  reply_settings = NULL
) {

  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    stop("`text` must be one string.", call. = FALSE)
  }

  token <- authenticate_user()

  body <- list(text = text)
  if (isTRUE(for_super_followers_only)) body$for_super_followers_only <- TRUE
  if (isTRUE(nullcast)) body$nullcast <- TRUE
  body$geo            <- geo
  body$media          <- media
  body$poll           <- poll
  body$reply          <- reply
  body$reply_settings <- reply_settings

  response <- x_request(token$access_token) |>
    req_url_path_append("tweets") |>
    req_method("POST") |>
    req_body_json(body) |>
    x_perform()

  invisible(response$data)
}
