#' @param user_fields \code{character}, \code{vector}; the fields to return
#'   for each user. Default: \code{c("created_at", "description", "protected",
#'   "entities", "location", "profile_image_url", "profile_banner_url",
#'   "public_metrics", "verified", "verified_type",
#'   "verified_followers_count", "subscription_type", "parody",
#'   "is_identity_verified", "url")}. Four more fields,
#'   \code{connection_status}, \code{confirmed_email},
#'   \code{receives_your_dm} and \code{subscribes_to_you}, describe the
#'   account's relationship with the signed-in user; they need a user token
#'   and are not requested by default.
