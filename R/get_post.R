#' Get Post by IDs
#'
#' @description
#' Returns a variety of information about the Post specified by the requested ID
#' via the [get posts by IDs endpoint](https://docs.x.com/x-api/posts/get-posts-by-ids).
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_path_append req_perform resp_body_json req_url_query
#' @importFrom stringr str_c
#' @param post_ids The IDs of the post to retrieve, stored in a list.
#' @template bearer_token
#' @template post_fields
#' @template user_fields
#' @return A list containing the retrieved post and any expansions.
#' @examples
#' \dontrun{
#' post <- get_post(c("1234567890123456789"))
#' }
#' @export
get_post <- function(
  post_ids,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  post_fields      =
      c("created_at", "text", "public_metrics", "geo", "attachments",
        "context_annotations", "entities", "lang", "referenced_tweets",
        "reply_settings", "conversation_id", "in_reply_to_user_id", "author_id",
        "edit_history_tweet_ids", "id"),
  user_fields      =
    c("created_at", "description", "protected", "entities", "location",
      "profile_image_url", "public_metrics", "verified", "verified_type"),
  media_fields     =
    c("duration_ms", "height", "width", "preview_image_url", "type", "url",
      "public_metrics", "variants", "media_key"),
  poll_fields      =
    c("end_datetime", "duration_minutes", "options", "voting_status", "id"),
  place_fields     =
    c("contained_within", "country", "country_code", "full_name", "geo", "id",
      "name", "place_type"),
  expansions       =
    c("author_id", "entities.mentions.username",
      "referenced_tweets.id.author_id", "referenced_tweets.id",
      "in_reply_to_user_id", "attachments.media_keys", "attachments.poll_ids",
      "geo.place_id")
) {

  # Join fields as comma-separated strings
  post_ids_str     <- str_c(post_ids, collapse = ",")
  post_fields_str  <- str_c(post_fields, collapse = ",")
  user_fields_str  <- str_c(user_fields, collapse = ",")
  media_fields_str <- str_c(media_fields, collapse = ",")
  poll_fields_str  <- str_c(poll_fields, collapse = ",")
  place_fields_str <- str_c(place_fields, collapse = ",")
  expansions_str   <- str_c(expansions, collapse = ",")

  while (TRUE) {
    tryCatch(
      expr = {
        request(base_url = "https://api.x.com/2") |>
          req_url_path_append(
            endpoint = paste0("tweets")
          ) |>
          req_url_query(
            ids          = post_ids_str,
            tweet.fields = post_fields_str,
            user.fields  = user_fields_str,
            media.fields = media_fields_str,
            poll.fields  = poll_fields_str,
            place.fields = place_fields_str,
            expansions   = expansions_str
          ) |>
          req_auth_bearer_token(token = bearer_token) |>
          req_perform() |>
          resp_body_json() ->
          this_post

        break
      },
      error = function(e) {
        message(e$message, " Retrying in 60 seconds.")
        Sys.sleep(60)
      }
    )
  }

  # Wrap response in timeline-compatible format
  wrapped_post <- list(
    data = this_post$data,
    includes = list(
      tweets = this_post$includes$tweets %||% list()
    )
  )

  return(list(wrapped_post))
}
