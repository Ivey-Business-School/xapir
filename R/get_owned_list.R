#' Get Owned List
#' 
#' @description
#' Get a User’s Owned Lists via the [owned list endpoint](https://docs.x.com/x-api/lists/get-a-users-owned-lists).
#'
#' @importFrom httr2 oauth_client oauth_flow_auth_code request req_auth_bearer_token req_perform resp_body_json
#' @importFrom tibble tibble
#' @importFrom purrr map_chr map_int map_lgl
#' @param username Username of the account that owns the lists
#' @template bearer_token 
#' @return A tibble containing the IDs of the lists and their names, or NULL if none found.
#' @examples
#' \dontrun{
#' lists <- get_owned_list(username = "Tesla")
#' }
#' @export
get_owned_list <- function(
  username,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  list_fields     =
    c("id", "name", "created_at", "description", "follower_count", "member_count", 
    "private")
) {

  # Get user ID
  url <- paste0("https://api.twitter.com/2/users/by/username/", username)
  
  req <- request(url) |>
    req_auth_bearer_token(bearer_token) |>
    req_perform()
  
  resp <- resp_body_json(req)

  if (!is.null(resp$error)) {
    stop("Error: ", resp$error)
  }

  user_id <- resp$data$id

  # Get lists owned by user
  lists_req <- request(paste0("https://api.twitter.com/2/users/", user_id, "/owned_lists")) |>
    req_auth_bearer_token(bearer_token) |>
    req_url_query(`list.fields` = paste(list_fields, collapse = ",")) |>
    req_perform()
  
  lists_data <- resp_body_json(lists_req)

  if (!is.null(lists_data$data)) {
    data <- lists_data$data
    return(tibble(
      list_id             = map_chr(data, ~ as.character(.x$id)),
      list_name           = map_chr(data, ~ as.character(.x$name)),
      description         = map_chr(data, ~ .x$description %||% NA_character_),
      created_at          = map_chr(data, ~ .x$created_at %||% NA_character_),
      follower_count      = map_int(data, ~ .x$follower_count %||% NA_integer_),
      member_count        = map_int(data, ~ .x$member_count %||% NA_integer_),
      private             = map_lgl(data, ~ .x$private %||% NA)
    ))
  } else {
    message("No owned lists found.")
    return(NULL)
  }
}
