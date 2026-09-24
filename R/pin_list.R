#' Pin List
#'
#' @description
#' Pins a list to the top of the signed-in account's lists via the [pin list
#' endpoint](https://docs.x.com/x-api/users/pin-list). An account can pin up
#' to five lists. Needs a user token, so the first call opens a browser
#' window to sign in. The request is billed, so the function says what it
#' costs before it sends anything.
#'
#' @importFrom httr2 req_body_json req_method req_url_path_append
#' @param list_id The id of the list to pin, as a string.
#' @return Invisibly, the `data` list the API returns, `list(pinned = TRUE)`.
#'   Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' pin_list(list_id = "1146654567674912769")
#' }
#' @export
pin_list <- function(
  list_id
) {

  check_list_id(list_id)
  token   <- authenticate_user()
  user_id <- my_user_id(token)

  announce_request_cost("list_manage")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "pinned_lists") |>
    req_method("POST") |>
    req_body_json(list(list_id = list_id)) |>
    x_perform()

  invisible(response$data)
}

#' Unpin List
#'
#' @description
#' Unpins a list from the signed-in account's lists via the [unpin list
#' endpoint](https://docs.x.com/x-api/users/unpin-list). Needs a user token,
#' so the first call opens a browser window to sign in. The request is
#' billed, so the function says what it costs before it sends anything.
#'
#' @importFrom httr2 req_method req_url_path_append
#' @param list_id The id of the list to unpin, as a string.
#' @return Invisibly, the `data` list the API returns, `list(pinned =
#'   FALSE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' unpin_list(list_id = "1146654567674912769")
#' }
#' @export
unpin_list <- function(
  list_id
) {

  check_list_id(list_id)
  token   <- authenticate_user()
  user_id <- my_user_id(token)

  announce_request_cost("list_manage")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "pinned_lists", list_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}

#' Get Pinned Lists
#'
#' @description
#' Retrieves the lists the signed-in account has pinned via the [get pinned
#' lists endpoint](https://docs.x.com/x-api/users/get-users-pinned-lists).
#' Needs a user token, so the first call opens a browser window to sign in.
#' An account can pin at most five lists and every list returned is billed,
#' so the function says what the call can cost before it reads anything.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @param list_fields \code{character}, \code{vector}; the fields to return
#'   for each list.
#' @return A tibble with one row per pinned list: `list_id`, `list_name`,
#'   `description`, `created_at` (POSIXct, UTC), `follower_count`,
#'   `member_count`, `private` and `owner_id`. When nothing is pinned, the
#'   same columns with no rows.
#' @examples
#' \dontrun{
#' pinned <- get_pinned_lists()
#' }
#' @export
get_pinned_lists <- function(
  list_fields = c(
    "id", "name", "created_at", "description", "follower_count",
    "member_count", "private", "owner_id"
  )
) {

  token   <- authenticate_user()
  user_id <- my_user_id(token)

  message(sprintf(
    "Reading up to 5 lists, about $%s (your own data). Five is the most an account can pin.",
    dollars(5 * x_price("owned"))
  ))

  page <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "pinned_lists") |>
    req_url_query(list.fields = join_fields(list_fields)) |>
    x_perform()

  warn_partial_errors(page$errors, what = "lists")
  lists_table(page$data)
}
