#' Get List Members
#'
#' @description
#' Returns a list of Users that are members of a List by the provided List ID via the [get list 
#' members](https://docs.x.com/x-api/users/returns-user-objects-that-are-members-of-a-list-by-the-provided-list-id).
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_query req_perform resp_body_json
#' @importFrom stringr str_c
#' @param list_id A string representing the unique ID of the list.
#' @template bearer_token 
#' @template user_fields
#' @return A list containing the API response.
#' @examples
#' \dontrun{
#' list_members <- get_list_members(list_ID)
#' }
#' @export
get_list_member <- function(
    list_id, 
    bearer_token = Sys.getenv("X_BEARER_TOKEN"),
    user_fields      =
      c("created_at", "description", "protected", "entities", "location",
        "profile_image_url", "public_metrics", "verified", "verified_type")
) {

  url <- paste0("https://api.twitter.com/2/lists/", list_id, "/members")
  user_fields_str <- str_c(user_fields, collapse = ",")

  resp <- request(url) |>
    req_url_query(user.fields = user_fields_str) |>
    req_auth_bearer_token(bearer_token) |>
    req_perform()

  json <- resp_body_json(resp)

  return(list(
    list(
      includes = list(users = json$data %||% list()),
      meta = json$meta %||% list()
  )))
}
