## Internal helpers shared by every function that needs a user token.
## Nothing in this file is exported.

#' @importFrom httr2 oauth_client oauth_flow_auth_code oauth_token_cached
NULL

# Scopes ---------------------------------------------------------------------

# Every permission the package asks for when a user signs in, one per line so
# the list is easy to read against the docs. A function only works when its
# scope is here, so add the scope when you add an endpoint. Changing the list
# does not touch a cached token: delete the cache (see authenticate_user())
# and sign in again to pick the new scopes up.
x_oauth_scopes <- c(
  "tweet.read",            # read posts
  "tweet.write",           # create_post(), delete_post(), reposts
  "tweet.moderate.write",  # hide_reply()
  "users.read",            # read profiles, users/me
  "follows.read",          # who follows whom
  "follows.write",         # follow_user(), unfollow_user()
  "like.read",             # get_liked_posts(), get_liking_users()
  "like.write",            # like_post(), unlike_post()
  "list.read",             # get_owned_list(), get_list_member(), ...
  "list.write",            # create_list(), add_list_member(), pin_list(), ...
  "block.read",            # get_blocking()
  "block.write",           # block_user(), unblock_user()
  "mute.read",             # get_muting()
  "mute.write",            # mute_user(), unmute_user()
  "bookmark.read",         # get_bookmark()
  "bookmark.write",        # create_bookmark(), delete_bookmark()
  "media.write",           # upload_media()
  "space.read",            # get_spaces()
  "offline.access"         # a refresh token, so sign-in lasts across sessions
)

# The API wants the scopes as one space-separated string.
x_oauth_scope <- function() {
  paste(x_oauth_scopes, collapse = " ")
}

# Own user id ----------------------------------------------------------------

# The id of the account that signed in, from the users/me endpoint. Fetched
# once a session and remembered in .x_env, keyed by the access token so a
# different sign-in never reuses the previous account's id.
my_user_id <- function(token) {
  key    <- token$access_token
  cached <- .x_env$my_user_id
  if (!is.null(cached) && identical(cached$key, key)) {
    return(cached$id)
  }

  body <- x_request(key) |>
    req_url_path_append("users", "me") |>
    x_perform()

  id <- pluck(body, "data", "id")
  if (is.null(id)) {
    stop(
      "The API did not say which account is signed in. ",
      "Delete the cached token (see ?authenticate_user) and sign in again.",
      call. = FALSE
    )
  }

  .x_env$my_user_id <- list(key = key, id = id)
  id
}

# Arguments ------------------------------------------------------------------

# Ids are strings of digits. As numbers they lose digits, so a number stops.
check_post_id <- function(post_id, arg = "post_id") {
  ok <- is.character(post_id) && length(post_id) == 1 &&
    !is.na(post_id) && grepl("^[0-9]+$", post_id)
  if (!ok) {
    stop(
      "`", arg, "` must be one string of digits, such as \"1234567890\". ",
      "Keep ids as text: as numbers they lose digits.",
      call. = FALSE
    )
  }
  invisible(post_id)
}

# The package renamed "tweet" to "post". Functions that used to take
# `tweet_id` still accept it, with a warning, so old scripts keep working.
# Call as: post_id <- use_post_id(post_id, tweet_id)
use_post_id <- function(post_id, tweet_id) {
  if (!is.null(tweet_id)) {
    warning("`tweet_id` is deprecated, use `post_id`.", call. = FALSE)
    if (missing(post_id)) {
      post_id <- tweet_id
    }
  }
  if (missing(post_id)) {
    stop("`post_id` is missing. Give the id of the post as a string.",
         call. = FALSE)
  }
  check_post_id(post_id)
}

# Bookmarks only ever belong to the account that signed in, so the old
# `username` argument had no effect. It is accepted and ignored for now.
warn_username_ignored <- function(username, fn) {
  if (!is.null(username)) {
    warning(
      "`username` is ignored by ", fn, "(): bookmarks always belong to ",
      "the account that signed in. Drop the argument.",
      call. = FALSE
    )
  }
  invisible(NULL)
}
