#' Get Users by Usernames
#'
#' @description
#' Retrieves details of multiple Users by their usernames via 
#' the [get users by usernames endpoint](https://docs.x.com/x-api/users/get-users-by-usernames).
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_query req_perform resp_body_json
#' @param usernames A list of usernames. Up to 100 comma-separated usernames can be looked up using this endpoint.
#' @template bearer_token
#' @template user_fields
#' @return A tibble containing the user information 
#' @examples
#' \dontrun{
#' # Get basic user info for multiple users
#' get_users_by_usernames(
#' user_ids = c("Tesla", "elonmusk")
#' )
#' }
#' @export
get_users_by_usernames <- function(
  usernames,
  bearer_token     = Sys.getenv("X_BEARER_TOKEN"),
  user_fields      =
      c("created_at", "description", "protected", "entities", "location",
        "profile_image_url", "public_metrics", "verified", "verified_type"),
  expansions       = NULL
) {
  
  # Base endpoint URL
  url <- "https://api.twitter.com/2/users/by"
  
  # Prepare query parameters
  usernames_str <- str_c(usernames, collapse = ",")
  user_fields_str <- str_c(user_fields, collapse = ",")
  
  # Perform GET request
  response <- request(url) |>
    req_auth_bearer_token(bearer_token) |>
    req_url_query(
      usernames = usernames_str,
      user.fields = user_fields_str
    ) |>
    req_perform() |>
    resp_body_json()
  
  # Extract user data directly
  response |>
    unlist(recursive = FALSE) ->
    user_list

  # Define the variable order
  user_variable <- c(
    "created_at",
    "username",
    "name",
    "description",
    "followers_count",
    "following_count",
    "post_count",
    "listed_count",
    "like_count",
    "protected",
    "verified",
    "verified_type",
    "location",
    "profile_image_url",
    "link_in_bio",
    "user_id"
  )

  # Create the user tibble
  user_list |>
    map_dfr(
      ~ tibble(
        created_at        = .x$created_at,
        username          = .x$username,
        name              = .x$name,
        description       = .x$description %||% NA |> as.character(),
        followers_count   = .x$public_metrics$followers_count,
        following_count   = .x$public_metrics$following_count,
        post_count        = .x$public_metrics$tweet_count,
        listed_count      = .x$public_metrics$listed_count,
        like_count        = .x$public_metrics$like_count,
        protected         = .x$protected,
        verified          = .x$verified,
        verified_type     = .x$verified_type,
        location          = .x$location %||% NA |> as.character(),
        profile_image_url = .x$profile_image_url,
        link_in_bio       = .x$entities$url$urls |>
                              pluck(1, "display_url", .default = NA) |>
                              as.character(),
        user_id           = .x$id
      )
    ) |>
    mutate(created_at = ymd_hms(created_at)) |>
    distinct(user_id, .keep_all = TRUE) |>
    select(any_of(user_variable)) ->
    user

  return(user)
}