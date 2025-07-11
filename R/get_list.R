#' Get List
#' 
#' @description
#' Get a User’s Owned Lists via the [owned list endpoint](https://docs.x.com/x-api/lists/get-a-users-owned-lists).
#'
#' @importFrom httr2 oauth_client oauth_flow_auth_code request req_auth_bearer_token req_perform resp_body_json
#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @param username Username of the account that owns the lists
#' @template bearer_token 
#' @return A tibble containing the IDs of the lists and their names, or NULL if none found.
#' @examples
#' \dontrun{
#' lists <- get_list(username = "Tesla")
#' }
#' @export
get_list <- function(
  username,
  bearer_token = Sys.getenv("X_BEARER_TOKEN")
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
    req_perform()
  
  lists_data <- resp_body_json(lists_req)

  if (!is.null(lists_data$data)) {
    data <- lists_data$data
    return(tibble(
      id          = map_chr(data, "id"),
      name        = map_chr(data, "name")
    ))
  } else {
    message("No owned lists found.")
    return(NULL)
  }
}
