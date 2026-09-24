#' @param expansions \code{character}, \code{vector}; the related objects to
#'   return alongside each post. Default: \code{c("author_id",
#'   "entities.mentions.username", "referenced_tweets.id.author_id",
#'   "referenced_tweets.id", "in_reply_to_user_id", "attachments.media_keys",
#'   "attachments.poll_ids", "geo.place_id")}. The API also offers
#'   \code{"attachments.media_source_tweet"}, \code{"article.cover_media"},
#'   \code{"article.media_entities"} and \code{"edit_history_tweet_ids"};
#'   no table reads them, so they are left out. Expansions are free.
