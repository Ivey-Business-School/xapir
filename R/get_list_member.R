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
  bearer_token      = Sys.getenv("X_BEARER_TOKEN"),
  user_fields       =
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

  if (is.null(json$data) || length(json$data) == 0) {
    message("No users found in the list.")
    return(tibble())
  }

  users <- json$data

  users_tbl <- tibble(
    list_id          = map_chr(list_id),
    user_id          = map_chr(users, ~ as.character(.x$id)),
    name             = map_chr(users, ~ .x$name %||% NA_character_),
    username         = map_chr(users, ~ .x$username %||% NA_character_),
    created_at       = map_chr(users, ~ .x$created_at %||% NA_character_),
    description      = map_chr(users, ~ .x$description %||% NA_character_),
    protected        = map_lgl(users, ~ .x$protected %||% NA),
    is_verified      = map_lgl(users, ~ .x$verified %||% NA),
    verified_type    = map_chr(users, ~ .x$verified_type %||% NA_character_),
    location         = map_chr(users, ~ .x$location %||% NA_character_),
    profile_image_url= map_chr(users, ~ .x$profile_image_url %||% NA_character_),
    followers_count  = map_int(users, ~ .x$public_metrics$followers_count %||% NA_integer_),
    following_count  = map_int(users, ~ .x$public_metrics$following_count %||% NA_integer_),
    tweet_count      = map_int(users, ~ .x$public_metrics$tweet_count %||% NA_integer_),
    listed_count     = map_int(users, ~ .x$public_metrics$listed_count %||% NA_integer_)
  )

  return(users_tbl)
}
