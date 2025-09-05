#' Get Followed Lists
#'
#' @description
#' Retrieves the Lists followed by a given User via the 
#' [get followed lists endpoint](https://docs.x.com/x-api/users/get-followed-lists).
#'
#' @importFrom httr2 request req_auth_bearer_token req_perform resp_body_json
#' @importFrom tibble tibble
#' @importFrom purrr map_chr map_int map_lgl
#' @param username Username of the account whose followed lists are being retrieved.
#' @template bearer_token
#' @param list_fields Character vector of list fields to include in the response.
#'   Defaults to commonly useful fields.
#' @return A tibble containing the IDs of the followed lists and their metadata, 
#'   or NULL if none are found.
#' @examples
#' \dontrun{
#' lists <- get_followed_lists(username = "XDevelopers")
#' }
#' @export
get_followed_lists <- function(
  username,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  list_fields  =
    c("id", "name", "created_at", "description", 
      "follower_count", "member_count", "private")
) {

  # Step 1: Get user ID from username
  url <- paste0("https://api.twitter.com/2/users/by/username/", username)
  
  req <- request(url) |>
    req_auth_bearer_token(bearer_token) |>
    req_perform()
  
  resp <- resp_body_json(req)

  if (!is.null(resp$error)) {
    stop("Error: ", resp$error)
  }

  user_id <- resp$data$id

  # Step 2: Get followed lists
  lists_req <- request(paste0("https://api.twitter.com/2/users/", user_id, "/followed_lists")) |>
    req_auth_bearer_token(bearer_token) |>
    req_url_query(`list.fields` = paste(list_fields, collapse = ",")) |>
    req_perform()
  
  lists_data <- resp_body_json(lists_req)

  # Step 3: Return tibble or NULL
  if (!is.null(lists_data$data)) {
    data <- lists_data$data
    return(tibble(
      list_id        = map_chr(data, ~ as.character(.x$id)),
      list_name      = map_chr(data, ~ as.character(.x$name)),
      description    = map_chr(data, ~ .x$description %||% NA_character_),
      created_at     = map_chr(data, ~ .x$created_at %||% NA_character_),
      follower_count = map_int(data, ~ .x$follower_count %||% NA_integer_),
      member_count   = map_int(data, ~ .x$member_count %||% NA_integer_),
      private        = map_lgl(data, ~ .x$private %||% NA)
    ))
  } else {
    message("No followed lists found.")
    return(NULL)
  }
}
