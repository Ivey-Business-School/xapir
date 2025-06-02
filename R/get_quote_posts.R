#' Retrieve Quote Posts for a Given Post
#'
#' @description
#' Returns a variety of information about each Post that quotes the Post specified by the requested ID
#' via the [quote tweets endpoint](https://docs.x.com/x-api/posts/retrieve-posts-that-quote-a-post).
#'
#' @param post_id The ID of the tweet to retrieve quote tweets for.
#' @template max_results 
#' @param exclude A comma-separated list of the types of posts to exclude from
#'   the response (e.g., "retweets", "replies", or "retweets,replies").
#' @template pagination_token
#' @template post_fields
#' @template user_fields
#' @template bearer_token 
#' @return A list containing the API response.
#' @examples
#' \dontrun{
#' get_quote_posts(tweet_id = "20", max_results = 100)
#' }
#' @export
get_quote_posts <- function(
  post_id,
  max_results      = 100,
  exclude          = NULL,
  pagination_token = NULL,
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
        "in_reply_to_user_id", "attachments.media_keys", "attachments.poll_ids"),
  bearer_token = Sys.getenv("X_BEARER_TOKEN")
) {
  # Validate max_results
  if (max_results < 1 || max_results > 100) {
    stop("max_results must be between 1 and 100.")
  }

  # Join the fields with commas as the API expects
  post_fields_str  <- str_c(post_fields, collapse = ",")
  user_fields_str  <- str_c(user_fields, collapse = ",")
  media_fields_str <- str_c(media_fields, collapse = ",")
  poll_fields_str  <- str_c(poll_fields, collapse = ",")
  place_fields_str <- str_c(place_fields, collapse = ",")
  expansions_str   <- str_c(expansions, collapse = ",")

  # Build the request URL
  url <- paste0("https://api.twitter.com/2/tweets/", post_id, "/quote_tweets")

  response <- NULL

  request(url) |>
    req_url_query(
      max_results      = max_results,
      exclude          = exclude,
      pagination_token = pagination_token,
      tweet.fields     = post_fields_str,
      user.fields      = user_fields_str,
      media.fields     = media_fields_str,
      poll.fields      = poll_fields_str,
      place.fields     = place_fields_str,
      expansions       = expansions_str
    ) |>
    req_auth_bearer_token(token = bearer_token) |>
    req_perform() |>
    resp_body_json() ->
    this_response

  response <- c(response, list(this_response))

  return(response)
}
