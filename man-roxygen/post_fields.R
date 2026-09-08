#' @param post_fields \code{character}, \code{vector}; the fields to return
#'   for each post. The default asks for everything the twelve tables need:
#'   \code{c("created_at", "text", "note_tweet", "article", "public_metrics",
#'   "geo", "attachments", "context_annotations", "entities", "lang",
#'   "possibly_sensitive", "edit_controls", "referenced_tweets",
#'   "reply_settings", "conversation_id", "in_reply_to_user_id", "author_id",
#'   "edit_history_tweet_ids", "id")}. Fields are free; posts cost, so there
#'   is nothing to save by trimming this.
