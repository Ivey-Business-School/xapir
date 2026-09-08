#' Extract Post Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle post data,
#' including metadata such as likes, reposts, replies, and impressions.
#'
#' Long posts (over 280 characters) arrive from the API with a truncated
#' `text` and the full text in `note_tweet`. This function puts the full text
#' in `text` and marks the row with `is_long_post = TRUE`.
#'
#' A repost carries the original post's like, reply, quote, bookmark and
#' repost counts, so those five are set to `NA` on reposts. Its impression
#' count is its own and is kept.
#'
#' Each post appears once, even when it sits in one page's `data` and another
#' page's `includes$tweets`. The `data` copy wins.
#'
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param additional_cols A character vector of derived columns to add.
#'   `"post_type"` classifies each post as Thread, Post, Quote post, Reply or
#'   Repost. `"post_url"` builds the address
#'   `https://x.com/<username>/status/<post_id>` from the author's handle in
#'   `includes$users`, falling back to `https://x.com/i/web/status/<post_id>`
#'   when the author is not there.
#' @param tz The time zone for `created_at`. The API returns UTC, and the
#'   default keeps it. Pass `Sys.timezone()` or a name such as
#'   `"America/Toronto"` to convert.
#' @param include_referenced_posts Logical. Whether to include the posts in
#'   `includes$tweets` (the posts that were quoted, replied to or reposted).
#'   Defaults to TRUE.
#' @return A tibble with one row per post. `article_title` is present only
#'   when at least one post in the timeline is an X article.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post <- extract_post(timeline)
#' }
#' @export
extract_post <- function(
  timeline,
  additional_cols = c("post_type", "post_url"),
  tz = "UTC",
  include_referenced_posts = TRUE
) {

  posts <- post_list(timeline, include_referenced_posts)

  post_schema <- tibble(
    created_at          = as.POSIXct(character(0), tz = "UTC"),
    text                = character(0),
    is_long_post        = logical(0),
    lang                = character(0),
    possibly_sensitive  = logical(0),
    article_title       = character(0),
    impression_count    = integer(0),
    like_count          = integer(0),
    repost_count        = integer(0),
    quote_count         = integer(0),
    reply_count         = integer(0),
    bookmark_count      = integer(0),
    reply_settings      = character(0),
    reposted            = character(0),
    quoted              = character(0),
    replied_to          = character(0),
    in_reply_to_user_id = character(0),
    user_id             = character(0),
    conversation_id     = character(0),
    post_id             = character(0)
  )

  if (length(posts) == 0) {
    post <- post_schema
  } else {
    post <- map_dfr(posts, post_row) |>
      # A post can sit in one page's data and another page's includes. Keep
      # the first copy, which is the data copy because post_list() puts data
      # first.
      distinct(post_id, .keep_all = TRUE) |>
      mutate(created_at = ymd_hms(created_at, tz = "UTC"))
  }

  post <- post |>
    mutate(created_at = with_tz(created_at, tzone = tz))

  # A repost's engagement counts belong to the original post. Its impressions
  # are its own, so they stay.
  post <- post |>
    mutate(
      across(
        .cols = c(like_count, reply_count, quote_count, bookmark_count,
                  repost_count),
        .fns  = ~ if_else(!is.na(reposted), NA_integer_, .x)
      )
    )

  # article_title only earns a column when the timeline holds an article.
  if (all(is.na(post$article_title))) {
    post <- select(post, -article_title)
  }

  if ("post_type" %in% additional_cols) {
    post <- add_post_type(post)
  }

  if ("post_url" %in% additional_cols) {
    post <- post |>
      left_join(author_lookup(timeline), by = "user_id") |>
      mutate(
        post_url = if_else(
          is.na(username),
          str_c("https://x.com/i/web/status/", post_id),
          str_c("https://x.com/", username, "/status/", post_id)
        )
      ) |>
      select(-username) |>
      relocate(post_url, .before = post_id)
  }

  return(post)
}

#' One row of the post table from one parsed post
#' @keywords internal
#' @noRd
post_row <- function(x) {
  refs     <- x$referenced_tweets %||% list()
  ref_type <- map_chr(refs, ~ .x$type %||% NA_character_)
  ref_id   <- map_chr(refs, ~ .x$id %||% NA_character_)
  # character(0)[1] is NA_character_, which is what we want when a type is
  # absent. A post can reply to one post and quote another, so each type is
  # read on its own rather than unnested into rows.
  ref_of <- function(type) ref_id[ref_type == type][1]

  metrics <- x$public_metrics %||% list()

  tibble(
    created_at          = x$created_at %||% NA_character_,
    text                = x$note_tweet$text %||% x$text %||% NA_character_,
    is_long_post        = !is.null(x$note_tweet),
    lang                = x$lang %||% NA_character_,
    possibly_sensitive  = x$possibly_sensitive %||% NA,
    article_title       = x$article$title %||% NA_character_,
    impression_count    = metrics$impression_count %||% NA_integer_,
    like_count          = metrics$like_count %||% NA_integer_,
    repost_count        = metrics$retweet_count %||% NA_integer_,
    quote_count         = metrics$quote_count %||% NA_integer_,
    reply_count         = metrics$reply_count %||% NA_integer_,
    bookmark_count      = metrics$bookmark_count %||% NA_integer_,
    reply_settings      = x$reply_settings %||% NA_character_,
    reposted            = ref_of("retweeted"),
    quoted              = ref_of("quoted"),
    replied_to          = ref_of("replied_to"),
    in_reply_to_user_id = x$in_reply_to_user_id %||% NA_character_,
    user_id             = x$author_id %||% NA_character_,
    conversation_id     = x$conversation_id %||% NA_character_,
    post_id             = x$id %||% NA_character_
  )
}

#' Add the post_type column
#'
#' @description
#' A thread is a run of posts in which the author replies to their own
#' conversation opener. The opener and every self-reply in the run are
#' labelled Thread; everything else is labelled by its reference type.
#'
#' @keywords internal
#' @noRd
add_post_type <- function(post) {
  post |>
    # Chronological order. created_at is safer than the id, whose digit count
    # changed over the years.
    arrange(created_at, post_id) |>
    # Cluster the posts at the user-conversation level
    group_by(user_id, conversation_id) |>
    mutate(
      is_first_post = post_id == first(post_id),
      is_self_reply = in_reply_to_user_id == user_id
    ) |>
    # Within each cluster, keep the first post at the top, elevate replies to
    # self, and sort the rest by the post they replied to.
    arrange(
      desc(is_first_post),
      desc(is_self_reply),
      replied_to,
      .by_group = TRUE
    ) |>
    mutate(
      is_thread = if_else(
        condition =
          # For user-conversation groups with more than one post...
          n() > 1 &
          # that were started by the user, set is_thread to TRUE if...
          conversation_id %in% post_id &
          # the user replied to their last post or the post is a self-reply
          (post_id == lead(replied_to) | replied_to == lag(post_id)),
        true    = TRUE,
        false   = FALSE,
        missing = FALSE
      )
    ) |>
    ungroup() |>
    mutate(
      post_type = case_when(
        is_thread          ~ "Thread",
        !is.na(replied_to) ~ "Reply",
        !is.na(quoted)     ~ "Quote post",
        !is.na(reposted)   ~ "Repost",
        TRUE               ~ "Post"
      ) |>
        factor(levels = c("Thread", "Post", "Quote post", "Reply", "Repost"))
    ) |>
    relocate(post_type, .before = impression_count) |>
    select(-is_first_post, -is_self_reply, -is_thread)
}
