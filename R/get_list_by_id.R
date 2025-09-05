#' Get List by ID
#'
#' @description
#' Retrieves the details of a specific List by its ID via the
#' [Get List by ID endpoint](https://docs.x.com/x-api/lists/get-list-by-id).
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_query req_perform resp_body_json
#' @param list_id The ID of the List to retrieve.
#' @template bearer_token
#' @param list_fields Character vector of fields to include (default common fields).
#' @return A tibble with list details (one row), or NULL if there's an error.
#' @examples
#' \dontrun{
#' lst <- get_list_by_id(list_id = "1146654567674912769")
#' }
#' @export
get_list_by_id <- function(
  list_id,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  list_fields   = c(
    "id", "name", "description", "created_at", "follower_count",
    "member_count", "owner_id", "private"
  )
) {

  # Prepare comma-separated query parameters
  query_params <- list(
    `list.fields` = paste(list_fields, collapse = ",")
  )

  # Perform the API request
  req <- request(paste0("https://api.x.com/2/lists/", list_id)) |>
    req_auth_bearer_token(bearer_token) |>
    req_url_query(!!!query_params) |>
    req_perform()

  resp <- resp_body_json(req)

  # Error handling
  if (!is.null(resp$errors)) {
    stop("API error: ", paste(sapply(resp$errors, `[[`, "detail"), collapse = "; "))
  }

  if (is.null(resp$data)) {
    message("No data returned for list ID: ", list_id)
    return(NULL)
  }

  ld <- resp$data

  # Convert to tibble
  library(tibble)
  library(dplyr)

  tib <- tibble(
    id             = ld$id,
    name           = ld$name %||% NA_character_,
    description    = ld$description %||% NA_character_,
    created_at     = ld$created_at %||% NA_character_,
    follower_count = ld$follower_count %||% NA_integer_,
    member_count   = ld$member_count %||% NA_integer_,
    owner_id       = ld$owner_id %||% NA_character_,
    private        = ld$private %||% NA
  )

  # Optionally attach includes as list-column for further inspection
  if (!is.null(resp$includes)) {
    tib$includes <- list(resp$includes)
  }

  tib
}
