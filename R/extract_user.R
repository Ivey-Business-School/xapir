#' Extract User Data from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to extract user metadata,
#' including profile details and public metrics.
#'
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured user data.
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr select distinct mutate any_of
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username    = "XDevelopers",
#'   max_results = 100,
#'   start_time  = iso_8601(Sys.Date() - 7)
#' )
#' user <- extract_user(timeline)
#' }
#' @export
extract_user <- function(timeline) {
  # Input validation
  if (is.null(timeline) || length(timeline) == 0) {
    warning("Timeline is empty or NULL")
    return(tibble())
  }
  
  # Extract user data with robust handling - accounting for nested structure
  user_list <- timeline |>
    # Remove NULL entries
    discard(is.null) |>
    # Flatten one level (each timeline entry contains a list with one element)
    map(~ if(is.list(.x) && length(.x) > 0) .x[[1]] else NULL) |>
    # Remove NULL results
    discard(is.null) |>
    # Check if entry has includes and users data
    keep(~ {
      includes <- pluck(.x, "includes", .default = NULL)
      users <- pluck(includes, "users", .default = NULL)
      !is.null(users) && length(users) > 0
    }) |>
    # Extract the users from includes
    map(~ pluck(.x, "includes", "users")) |>
    # Flatten the user lists
    unlist(recursive = FALSE)
  
  # Check if we have any user data
  if (length(user_list) == 0) {
    warning("No user data found in timeline")
    return(tibble())
  }
  
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
  
  # Create the user tibble with robust error handling
  user <- user_list |>
    # Filter out NULL or invalid user entries
    keep(~ !is.null(.x) && is.list(.x) && !is.null(pluck(.x, "id"))) |>
    map_dfr(
      ~ {
        # Safely extract public metrics
        public_metrics <- pluck(.x, "public_metrics", .default = list())
        
        # Safely extract URL from entities
        link_in_bio <- tryCatch({
          .x |>
            pluck("entities", "url", "urls", .default = list()) |>
            pluck(1, "display_url", .default = NA_character_) |>
            as.character()
        }, error = function(e) NA_character_)
        
        tibble(
          created_at = pluck(.x, "created_at", .default = NA_character_),
          username = pluck(.x, "username", .default = NA_character_),
          name = pluck(.x, "name", .default = NA_character_),
          description = pluck(.x, "description", .default = NA_character_) |> as.character(),
          followers_count = pluck(public_metrics, "followers_count", .default = NA_integer_),
          following_count = pluck(public_metrics, "following_count", .default = NA_integer_),
          post_count = pluck(public_metrics, "tweet_count", .default = NA_integer_),
          listed_count = pluck(public_metrics, "listed_count", .default = NA_integer_),
          like_count = pluck(public_metrics, "like_count", .default = NA_integer_),
          protected = pluck(.x, "protected", .default = NA),
          verified = pluck(.x, "verified", .default = NA),
          verified_type = pluck(.x, "verified_type", .default = NA_character_),
          location = pluck(.x, "location", .default = NA_character_) |> as.character(),
          profile_image_url = pluck(.x, "profile_image_url", .default = NA_character_),
          link_in_bio = link_in_bio,
          user_id = pluck(.x, "id", .default = NA_character_)
        )
      }
    )
  
  # Handle empty result
  if (nrow(user) == 0) {
    warning("No valid user data could be extracted")
    return(tibble())
  }
  
  # Process dates and clean up
  user <- user |>
    mutate(
      created_at = tryCatch(
        ymd_hms(created_at), 
        error = function(e) as.POSIXct(NA)
      )
    ) |>
    # Remove rows where user_id is NA (invalid entries)
    filter(!is.na(user_id)) |>
    distinct(user_id, .keep_all = TRUE) |>
    select(any_of(user_variable))
  
  return(user)
}
