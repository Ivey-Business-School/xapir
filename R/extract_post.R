#' Extract Post Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle post data,
#' including metadata such as likes, retweets, replies, and impressions.
#'
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @param additional_cols A list of extra columns to be included in the tibble
#' @param include_referenced_posts Logical. Whether to include referenced posts in the output. Defaults to TRUE.
#' @return A tibble containing structured post data.
#' @importFrom purrr map map_dfr map_chr pluck map_lgl
#' @importFrom dplyr mutate select any_of arrange distinct
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
  tz = Sys.timezone(),
  include_referenced_posts = TRUE
) {

  timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) ->
    post_list

  map_lgl(
    timeline,
    ~ "includes" %in% names(.x) && "tweets" %in% names(.x$includes)
  ) |>
    any() ->
    timeline_has_referenced_posts

  if (timeline_has_referenced_posts && include_referenced_posts) {
    timeline |>
      map(pluck("includes")) |>
      map(pluck("tweets")) |>
      unlist(recursive = FALSE) ->
      post_list_referenced
  } else {
    post_list_referenced <- NULL
  }

  # Combine post data
  post_list_all <- c(post_list, post_list_referenced)

  post_schema <- tibble(
    created_at          = NA_POSIXct_,
    text                = NA_character_,
    impression_count    = NA_integer_,
    like_count          = NA_integer_,
    repost_count        = NA_integer_,
    quote_count         = NA_integer_,
    reply_count         = NA_integer_,
    bookmark_count      = NA_integer_,
    reply_settings      = NA_character_,
    referenced_posts    = list(NULL),
    in_reply_to_user_id = NA_character_,
    user_id             = NA_character_,
    conversation_id     = NA_character_,
    post_id             = NA_character_
  )

  post_variable <- c(
    "created_at",
    "text",
    "impression_count",
    "like_count",
    "repost_count",
    "quote_count",
    "reply_count",
    "bookmark_count",
    "reply_settings",
    "reposted",
    "quoted",
    "replied_to",
    "in_reply_to_user_id",
    "user_id",
    "conversation_id",
    "post_id"
  )

  # Create the post tibble
  post_list_all |>
    map_dfr(
      ~ tibble(
        created_at          = .x$created_at,
        text                = .x$text,
        impression_count    = .x$public_metrics$impression_count,
        like_count          = .x$public_metrics$like_count,
        repost_count        = .x$public_metrics$retweet_count,
        quote_count         = .x$public_metrics$quote_count,
        reply_count         = .x$public_metrics$reply_count,
        bookmark_count      = .x$public_metrics$bookmark_count,
        reply_settings      = .x$reply_settings,
        referenced_posts    = .x$referenced_tweets,
        in_reply_to_user_id = .x$in_reply_to_user_id,
        user_id             = .x$author_id,
        conversation_id     = .x$conversation_id,
        post_id             = .x$id
      )
    ) ->
    post

  post |>
    add_column(
      !!!post_schema[setdiff(names(post_schema), names(post))]
    ) |>
    mutate(created_at = ymd_hms(created_at) |> with_tz(tz)) |>
    mutate(
      ref_type = map_chr(referenced_posts, ~ .x$type %||% NA_character_),
      ref_id   = map_chr(referenced_posts, ~ .x$id %||% NA_character_)
    ) |>
    mutate(
      reposted   = ifelse(ref_type == "retweeted", ref_id, NA_character_),
      .after = reply_settings
    ) |>
    mutate(
      reposted   = ifelse(ref_type == "retweeted", ref_id, NA_character_),
      reposted   = as.character(reposted),
      quoted     = ifelse(ref_type == "quoted", ref_id, NA_character_),
      quoted     = as.character(quoted),
      replied_to = ifelse(ref_type == "replied_to", ref_id, NA_character_),
      replied_to = as.character(replied_to),
      .after     = reply_settings
    ) |>
    select(all_of(post_variable)) ->
    post

    # Convert the engagement metrics to missing for reposts ------------------
post |> 
  mutate(
     across(
        .cols = ends_with("_count"), 
        .fns  = ~ ifelse(!is.na(reposted), NA_integer_, .x)
     )
     
  ) ->
  post

  if("post_type" %in% additional_cols) {
    # is_thread ---------------------------------------------------------------
    post |>
      # arrange the posts in chronological order
      arrange(post_id) |>
      # cluster the posts at the user-conversation level
      group_by(user_id, conversation_id) |>
      mutate(
        is_first_post = post_id == first(post_id),
        is_self_reply  = in_reply_to_user_id == user_id
      ) |>
      # Within each cluster, keep the first post at the top, elevate replies to
      # self, and sort replies to self / replies to others chronologically based on
      # the post_id values of the posts the user replied to.
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
          true      = TRUE,
          false     = FALSE,
          missing   = FALSE
        )
      ) |>
      ungroup() |>
      select(-is_first_post, -is_self_reply) ->
      post

    # post_type --------------------------------------------------------------
    post |>
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
      select(-is_thread) ->
      post
  }

  if("post_url" %in% additional_cols){
    # post_url ---------------------------------------------------
    post |> 
      mutate    (
        post_url = str_c("https://twitter.com/tesla/status/", post_id)
      ) |> 
      relocate(post_url, .before = post_id) ->
      post
  }

  return(post)
}
