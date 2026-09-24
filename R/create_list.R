#' Create List
#'
#' @description
#' Creates a list owned by the signed-in account via the [create list
#' endpoint](https://docs.x.com/x-api/lists/create-list). Needs a user
#' token, so the first call opens a browser window to sign in. The request
#' is billed, so the function says what it costs before it sends anything.
#'
#' @importFrom httr2 req_body_json req_method req_url_path_append
#' @param name The name of the list, 1 to 25 characters.
#' @param description An optional description, up to 100 characters.
#' @param private `TRUE` makes the list private. Default `FALSE`.
#' @return Invisibly, the `data` list the API returns, `list(id = "...",
#'   name = "...")`. Keep the `id`: every other list function takes it as
#'   `list_id`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' new_list <- create_list(name = "EV makers", description = "Who builds EVs")
#' new_list$id
#' }
#' @export
create_list <- function(
  name,
  description = NULL,
  private     = FALSE
) {

  if (missing(name)) {
    stop("`name` is missing. Give the list a name of 1 to 25 characters.",
         call. = FALSE)
  }
  check_list_name(name, required = TRUE)
  check_list_description(description)
  check_list_private(private, required = TRUE)

  token <- authenticate_user()

  announce_request_cost("list_create")

  response <- x_request(token$access_token) |>
    req_url_path_append("lists") |>
    req_method("POST") |>
    req_body_json(compact(list(
      name        = name,
      description = description,
      private     = private
    ))) |>
    x_perform()

  invisible(response$data)
}

#' Update List
#'
#' @description
#' Changes the name, description or privacy of a list the signed-in account
#' owns via the [update list
#' endpoint](https://docs.x.com/x-api/lists/update-list). Only the arguments
#' given are sent; the rest stay as they are. Needs a user token, so the
#' first call opens a browser window to sign in. The request is billed, so
#' the function says what it costs before it sends anything.
#'
#' @importFrom httr2 req_body_json req_method req_url_path_append
#' @param list_id The id of the list, as a string.
#' @param name A new name, 1 to 25 characters, or `NULL` to keep the old one.
#' @param description A new description, up to 100 characters, or `NULL`.
#' @param private `TRUE` or `FALSE` to change the privacy, or `NULL`.
#' @return Invisibly, the `data` list the API returns, `list(updated =
#'   TRUE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' update_list(list_id = "1146654567674912769", name = "EV makers 2026")
#' update_list(list_id = "1146654567674912769", private = TRUE)
#' }
#' @export
update_list <- function(
  list_id,
  name        = NULL,
  description = NULL,
  private     = NULL
) {

  check_list_id(list_id)
  check_list_name(name, required = FALSE)
  check_list_description(description)
  check_list_private(private, required = FALSE)
  if (is.null(name) && is.null(description) && is.null(private)) {
    stop(
      "Give at least one of `name`, `description` or `private` to change.",
      call. = FALSE
    )
  }

  token <- authenticate_user()

  announce_request_cost("list_manage")

  response <- x_request(token$access_token) |>
    req_url_path_append("lists", list_id) |>
    req_method("PUT") |>
    req_body_json(compact(list(
      name        = name,
      description = description,
      private     = private
    ))) |>
    x_perform()

  invisible(response$data)
}

#' Delete List
#'
#' @description
#' Deletes a list the signed-in account owns via the [delete list
#' endpoint](https://docs.x.com/x-api/lists/delete-list). Needs a user
#' token, so the first call opens a browser window to sign in. The request
#' is billed, so the function says what it costs before it sends anything.
#'
#' @importFrom httr2 req_method req_url_path_append
#' @param list_id The id of the list to delete, as a string.
#' @return Invisibly, the `data` list the API returns, `list(deleted =
#'   TRUE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' delete_list(list_id = "1146654567674912769")
#' }
#' @export
delete_list <- function(
  list_id
) {

  check_list_id(list_id)
  token <- authenticate_user()

  announce_request_cost("list_manage")

  response <- x_request(token$access_token) |>
    req_url_path_append("lists", list_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}

# The limits the API states for a list's name, description and privacy,
# checked before the browser opens or anything is billed.
check_list_name <- function(name, required = TRUE) {
  if (is.null(name)) {
    if (required) {
      stop("`name` must be one string of 1 to 25 characters.", call. = FALSE)
    }
    return(invisible(NULL))
  }
  ok <- is.character(name) && length(name) == 1 && !is.na(name) &&
    nchar(name) >= 1 && nchar(name) <= 25
  if (!ok) {
    stop("`name` must be one string of 1 to 25 characters.", call. = FALSE)
  }
  invisible(name)
}

check_list_description <- function(description) {
  if (is.null(description)) {
    return(invisible(NULL))
  }
  ok <- is.character(description) && length(description) == 1 &&
    !is.na(description) && nchar(description) <= 100
  if (!ok) {
    stop("`description` must be one string of up to 100 characters.",
         call. = FALSE)
  }
  invisible(description)
}

check_list_private <- function(private, required = TRUE) {
  if (is.null(private) && !required) {
    return(invisible(NULL))
  }
  ok <- is.logical(private) && length(private) == 1 && !is.na(private)
  if (!ok) {
    stop("`private` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(private)
}
