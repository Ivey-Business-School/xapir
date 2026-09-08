#' @keywords internal
"_PACKAGE"

## Imports shared by every file. Functions that only one file uses are
## imported in that file's roxygen block.
#' @importFrom rlang %||%
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map map_chr map_dfr map_lgl pluck compact
#' @importFrom dplyr mutate select filter distinct arrange left_join
#'   any_of all_of across ends_with if_else case_when group_by ungroup
#'   relocate lead lag first n desc bind_rows slice
#' @importFrom stringr str_c
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom stats setNames
#' @importFrom utils globalVariables
NULL

## Column names used inside dplyr verbs. Declaring them keeps R CMD check
## from reading them as undefined variables.
utils::globalVariables(c(
  "article_title", "bookmark_count", "conversation_id", "created_at",
  "edited_post_id", "end", "impression_count", "in_reply_to_user_id",
  "is_first_post", "is_self_reply", "is_thread", "like_count", "media_id",
  "place_id", "poll_id", "post_id", "post_type", "post_url", "quote_count",
  "replied_to", "reply_count", "repost_count", "start", "user_id", "username"
))
