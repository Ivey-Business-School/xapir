#' Get Muting
#'
#' @description
#' Retrieves a list of Users muted by the authenticated user via 
#' the [get muting endpoint](https://docs.x.com/x-api/users/get-muting).
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_query req_perform resp_body_json
#' @importFrom stringr str_c
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr select distinct mutate any_of
#' @importFrom tibble tibble
#' @importFrom lubridate ymd_hms
#' @template user_fields
#' @return A tibble containing the muted users
#' @examples
#' \dontrun{
#' # Get basic user info for multiple users
#' muted_users <- get_muting()
#' }
#' @export
get_muting <- function(
  user_fields = c("created_at", "description", "protected", "entities", "location",
                  "profile_image_url", "public_metrics", "verified", "verified_type")
) {
  # Get cached or refreshed token
  token <- authenticate_user()
  
  # Get authenticated user's ID first
  user_req <- request("https://api.twitter.com/2/users/me") |>
    req_auth_bearer_token(token$access_token) |>
    req_perform()
  
  user_data <- resp_body_json(user_req)
  user_id <- user_data$data$id
  
  # Base endpoint URL for muting
  url <- paste0("https://api.twitter.com/2/users/", user_id, "/muting")
  
  # Prepare query parameters
  user_fields_str <- str_c(user_fields, collapse = ",")
  
  # Perform GET request
  response <- request(url) |>
    req_auth_bearer_token(token$access_token) |>
    req_url_query(
      user.fields = user_fields_str
    ) |>
    req_perform() |>
    resp_body_json()
  
  # Extract user data directly from response$data
  user_list <- response$data %||% list()
  
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
        created_at = .x$created_at,
        username = .x$username,
        name = .x$name,
        description = .x$description %||% NA |> as.character(),
        followers_count = .x$public_metrics$followers_count,
        following_count = .x$public_metrics$following_count,
        post_count = .x$public_metrics$tweet_count,
        listed_count = .x$public_metrics$listed_count,
        like_count = .x$public_metrics$like_count,
        protected = .x$protected,
        verified = .x$verified,
        verified_type = .x$verified_type,
        location = .x$location %||% NA |> as.character(),
        profile_image_url = .x$profile_image_url,
        link_in_bio = .x$entities$url$urls |>
          pluck(1, "display_url", .default = NA) |>
          as.character(),
        user_id = .x$id
      )
    ) |>
    mutate(created_at = ymd_hms(created_at)) |>
    distinct(user_id, .keep_all = TRUE) |>
    select(any_of(user_variable)) ->
    user
  
  return(user)
}