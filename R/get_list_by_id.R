#' Get List by ID
#'
#' @description
#' Retrieves the details of one list by its id via the
#' [get list by ID endpoint](https://docs.x.com/x-api/lists/get-list-by-id).
#' An id the API cannot find stops the call with the API's reason.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr map_chr
#' @param list_id The list's id, as a string of digits.
#' @template bearer_token
#' @param list_fields \code{character}, \code{vector}; the fields to return
#'   for the list.
#' @return A tibble with one row: `list_id`, `list_name`, `description`,
#'   `created_at` (POSIXct, UTC), `follower_count`, `member_count`, `private`
#'   and `owner_id`.
#' @examples
#' \dontrun{
#' lst <- get_list_by_id(list_id = "1146654567674912769")
#' }
#' @export
get_list_by_id <- function(
  list_id,
  bearer_token = Sys.getenv("X_BEARER_TOKEN"),
  list_fields  = c(
    "id", "name", "description", "created_at", "follower_count",
    "member_count", "owner_id", "private"
  )
) {
  check_token(bearer_token)
  check_list_id(list_id)

  page <- x_request(bearer_token) |>
    req_url_path_append("lists", list_id) |>
    req_url_query(list.fields = join_fields(list_fields)) |>
    x_perform()

  # A list that does not exist comes back as a 200 with `errors` and no
  # `data`, the same way an unknown handle does.
  if (is.null(page$data)) {
    reason <- map_chr(page$errors %||% list(), ~ .x$detail %||% .x$title %||% "")
    stop(
      "No list found for list_id \"", list_id, "\". ",
      paste(reason, collapse = " "),
      call. = FALSE
    )
  }

  # This endpoint returns one list object in `data`, not a list of them.
  lists_table(list(page$data))
}
