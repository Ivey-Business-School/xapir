#' @param user_fields \code{character}, \code{vector}; the fields to return
#'   for each user. Default: \code{c("created_at", "description", "protected",
#'   "entities", "location", "profile_image_url", "profile_banner_url",
#'   "public_metrics", "verified", "verified_type",
#'   "is_identity_verified", "url")}. Three fields the spec lists,
#'   \code{verified_followers_count}, \code{subscription_type} and
#'   \code{parody}, are refused to an app token ("not authorized to access
#'   'parody' on the user", 24 September 2026), so they are not requested by
#'   default; their columns are NA. Ask for them with
#'   \code{user_fields = c(default_user_fields(), "parody")} when your
#'   token can read them. Four more, \code{connection_status},
#'   \code{confirmed_email}, \code{receives_your_dm} and
#'   \code{subscribes_to_you}, describe the account's relationship with the
#'   signed-in user and need a user token.
