#' Create Post
#'
#' @description
#' Publishes a post from the signed-in account via the [create a post
#' endpoint](https://docs.x.com/x-api/posts/create-post). Needs a user
#' token, so the first call opens a browser window to sign in.
#'
#' The plain arguments (`media_ids`, `quote_post_id`, `reply_to_post_id`,
#' `poll_options`, ...) cover the everyday cases. The list arguments
#' (`media`, `poll`, `reply`, `geo`) take the API's own nested objects for
#' anything the plain ones do not reach, such as tagging users in a photo.
#' Give one or the other for each part, not both.
#'
#' @details
#' A post is billed per request, and a post whose text contains a link costs
#' more: the pricing page lists "Post: Create" at $0.015 and "Post: Create
#' (with URL)" at $0.200, more than thirteen times as much. The cost line
#' printed before the request shows which rate applies, so check it before
#' posting a batch with links. A link is anything that looks like
#' `https://...`, `http://...` or `www....`.
#'
#' Upload a photo, GIF or video with [upload_media()] first and pass the id
#' it returns as `media_ids`. A post can carry up to 4 photos, 1 GIF or 1
#' video, and the docs say media cannot be combined with a poll.
#'
#' The docs also say quote posts (`quote_post_id`) need an Enterprise plan;
#' on a pay-per-use account the API refuses them with its own message.
#'
#' @importFrom httr2 req_body_json req_method req_url_path_append
#' @param text The text of the post. The maximum length depends on the
#'   account's tier (280 characters on a standard account, longer on
#'   Premium). The API refuses text that is too long and the error says so.
#' @param media_ids Ids of media uploaded with [upload_media()], as strings:
#'   up to 4 photos, or 1 GIF, or 1 video. Cannot be combined with a poll.
#' @param quote_post_id The id of a post to quote, as a string.
#' @param reply_to_post_id The id of the post to reply to, as a string.
#' @param poll_options Two to four choices for a poll, each 1 to 25
#'   characters. Cannot be combined with media.
#' @param poll_duration_minutes How long the poll runs, 5 to 10,080 minutes
#'   (a week). The default is a day.
#' @param community_id The id of an X community to post in, as a string.
#' @param paid_partnership `TRUE` to label the post as a paid partnership.
#' @param share_with_followers `TRUE` to share a super-followers-only post
#'   with all followers.
#' @param for_super_followers_only `TRUE` to show the post only to super
#'   followers.
#' @param geo A list with a `place_id`, to attach a place to the post.
#' @param media A list with `media_ids` (and optionally `tagged_user_ids`),
#'   the API's own media object, for what `media_ids` does not cover.
#' @param nullcast `TRUE` for a promoted-only post that does not appear in
#'   the public timeline.
#' @param poll A list with `options` and `duration_minutes`, the API's own
#'   poll object, for what `poll_options` does not cover.
#' @param reply A list with `in_reply_to_tweet_id` (and optionally
#'   `exclude_reply_user_ids`), the API's own reply object, for what
#'   `reply_to_post_id` does not cover.
#' @param reply_settings Who can reply: `"following"`, `"mentionedUsers"`,
#'   `"subscribers"` or `"verified"`. Leave `NULL` to let everyone reply.
#' @return Invisibly, the `data` list the API returns, with the new post's
#'   `id` and `text`. Stops with the API's message when the post is refused.
#' @examples
#' \dontrun{
#' new_post <- create_post(text = "Hello, world!")
#' new_post$id
#'
#' # A photo, uploaded first
#' media_id <- upload_media("chart.png", alt_text = "Sales by month")
#' create_post("Our year so far", media_ids = media_id)
#'
#' # A reply, a quote, a poll
#' create_post("Agreed!", reply_to_post_id = new_post$id)
#' create_post("Worth a read", quote_post_id = "1234567890123456789")
#' create_post("Which one?", poll_options = c("This", "That"),
#'             poll_duration_minutes = 60)
#' }
#' @export
create_post <- function(
  text,
  media_ids = NULL,
  quote_post_id = NULL,
  reply_to_post_id = NULL,
  poll_options = NULL,
  poll_duration_minutes = 1440,
  community_id = NULL,
  paid_partnership = FALSE,
  share_with_followers = FALSE,
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

  # Each part comes either as a plain argument or as the API's list, not both.
  check_one_form(media, media_ids, "media", "media_ids")
  check_one_form(poll, poll_options, "poll", "poll_options")
  check_one_form(reply, reply_to_post_id, "reply", "reply_to_post_id")

  if (!is.null(media_ids)) {
    check_post_ids(media_ids, max_ids = 4, arg = "media_ids")
    media <- list(media_ids = as.list(media_ids))
  }
  if (!is.null(poll_options)) {
    check_poll(poll_options, poll_duration_minutes)
    poll <- list(
      options          = as.list(poll_options),
      duration_minutes = as.integer(poll_duration_minutes)
    )
  }
  if (!is.null(reply_to_post_id)) {
    check_post_id(reply_to_post_id, arg = "reply_to_post_id")
    reply <- list(in_reply_to_tweet_id = reply_to_post_id)
  }
  if (!is.null(quote_post_id)) {
    check_post_id(quote_post_id, arg = "quote_post_id")
  }
  if (!is.null(community_id)) {
    check_post_id(community_id, arg = "community_id")
  }
  if (!is.null(media) && !is.null(poll)) {
    stop(
      "A post cannot carry both media and a poll: the docs say media ",
      "cannot be combined with a poll. Drop one of them.",
      call. = FALSE
    )
  }
  check_reply_settings(reply_settings)

  token <- authenticate_user()

  body <- list(text = text)
  if (isTRUE(for_super_followers_only)) body$for_super_followers_only <- TRUE
  if (isTRUE(nullcast)) body$nullcast <- TRUE
  if (isTRUE(paid_partnership)) body$paid_partnership <- TRUE
  if (isTRUE(share_with_followers)) body$share_with_followers <- TRUE
  body$community_id   <- community_id
  body$geo            <- geo
  body$media          <- media
  body$poll           <- poll
  body$quote_tweet_id <- quote_post_id
  body$reply          <- reply
  body$reply_settings <- reply_settings

  # A post with a link is billed at a much higher rate than one without.
  if (has_url(text)) {
    announce_request_cost("post_create_with_url")
  } else {
    announce_request_cost("post_create")
  }

  response <- x_request(token$access_token) |>
    req_url_path_append("tweets") |>
    req_method("POST") |>
    req_body_json(body) |>
    x_perform()

  invisible(response$data)
}

# TRUE when the text holds something the API bills as a URL.
has_url <- function(text) {
  grepl("https?://|\\bwww\\.", text, ignore.case = TRUE)
}

# The list argument and its plain counterpart cannot both be given.
check_one_form <- function(list_arg, plain_arg, list_name, plain_name) {
  if (!is.null(list_arg) && !is.null(plain_arg)) {
    stop(
      "Give either `", plain_name, "` or `", list_name, "`, not both.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# Two to four options of 1 to 25 characters, running 5 minutes to a week.
check_poll <- function(poll_options, poll_duration_minutes) {
  ok <- is.character(poll_options) && !anyNA(poll_options) &&
    length(poll_options) >= 2 && length(poll_options) <= 4 &&
    all(nchar(poll_options) >= 1 & nchar(poll_options) <= 25)
  if (!ok) {
    stop(
      "`poll_options` must be 2 to 4 strings of 1 to 25 characters each.",
      call. = FALSE
    )
  }
  ok <- is.numeric(poll_duration_minutes) && length(poll_duration_minutes) == 1 &&
    !is.na(poll_duration_minutes) && poll_duration_minutes >= 5 &&
    poll_duration_minutes <= 10080
  if (!ok) {
    stop(
      "`poll_duration_minutes` must be a number between 5 and 10,080 ",
      "(one week).",
      call. = FALSE
    )
  }
  invisible(NULL)
}

check_reply_settings <- function(reply_settings) {
  if (is.null(reply_settings)) {
    return(invisible(NULL))
  }
  choices <- c("following", "mentionedUsers", "subscribers", "verified")
  ok <- is.character(reply_settings) && length(reply_settings) == 1 &&
    !is.na(reply_settings) && reply_settings %in% choices
  if (!ok) {
    stop(
      "`reply_settings` must be one of ",
      paste0("\"", choices, "\"", collapse = ", "),
      ", or NULL to let everyone reply.",
      call. = FALSE
    )
  }
  invisible(reply_settings)
}
