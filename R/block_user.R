#' Block User
#'
#' @description
#' Blocks an account from the signed-in account via the [block user
#' endpoint](https://docs.x.com/x-api/users/block-user). Needs a user token,
#' so the first call opens a browser window to sign in. The request is
#' billed as one interaction, so the function says what it costs before it
#' sends anything.
#'
#' Give either `target_username` or `target_user_id`, not both. A handle
#' costs one user read to turn it into an id before the block is sent; pass
#' the id when you already know it and that read is skipped.
#'
#' The docs list this endpoint as Enterprise only, so on a pay-per-use tier
#' the API may refuse it. The function passes the API's message on.
#'
#' @importFrom httr2 req_body_json req_method req_url_path_append
#' @param target_username Username of the account to block, without the "@"
#'   symbol.
#' @param target_user_id The id of the account to block, as a string of
#'   digits. When given, `target_username` must be `NULL`.
#' @return Invisibly, the `data` list the API returns, `list(blocking =
#'   TRUE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' block_user(target_username = "spammer")
#' block_user(target_user_id = "2244994945")
#' }
#' @export
block_user <- function(
  target_username = NULL,
  target_user_id  = NULL
) {

  check_target_user(target_username, target_user_id)
  token          <- authenticate_user()
  user_id        <- my_user_id(token)
  target_user_id <- resolve_target_user(target_username, target_user_id, token)

  announce_request_cost("interaction")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "blocking") |>
    req_method("POST") |>
    req_body_json(list(target_user_id = target_user_id)) |>
    x_perform()

  invisible(response$data)
}

#' Unblock User
#'
#' @description
#' Removes a block from the signed-in account via the [unblock user
#' endpoint](https://docs.x.com/x-api/users/unblock-user). Needs a user
#' token, so the first call opens a browser window to sign in. The request
#' is billed, so the function says what it costs before it sends anything.
#'
#' Give either `target_username` or `target_user_id`, not both. A handle
#' costs one user read to turn it into an id before the request is sent.
#'
#' The docs list this endpoint as Enterprise only, so on a pay-per-use tier
#' the API may refuse it. The function passes the API's message on.
#'
#' @importFrom httr2 req_method req_url_path_append
#' @param target_username Username of the account to unblock, without the
#'   "@" symbol.
#' @param target_user_id The id of the account to unblock, as a string of
#'   digits. When given, `target_username` must be `NULL`.
#' @return Invisibly, the `data` list the API returns, `list(blocking =
#'   FALSE)`. Stops with the API's message when the request is refused.
#' @examples
#' \dontrun{
#' unblock_user(target_username = "spammer")
#' }
#' @export
unblock_user <- function(
  target_username = NULL,
  target_user_id  = NULL
) {

  check_target_user(target_username, target_user_id)
  token          <- authenticate_user()
  user_id        <- my_user_id(token)
  target_user_id <- resolve_target_user(target_username, target_user_id, token)

  announce_request_cost("interaction_delete")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", user_id, "blocking", target_user_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}

# Exactly one of the two ways to name the other account, checked before the
# browser opens or anything is billed.
check_target_user <- function(target_username, target_user_id) {
  if (is.null(target_username) == is.null(target_user_id)) {
    stop(
      "Give either `target_username` or `target_user_id`, not both and ",
      "not neither.",
      call. = FALSE
    )
  }
  if (!is.null(target_user_id)) {
    ok <- is.character(target_user_id) && length(target_user_id) == 1 &&
      !is.na(target_user_id) && grepl("^[0-9]+$", target_user_id)
    if (!ok) {
      stop(
        "`target_user_id` must be one string of digits, such as ",
        "\"2244994945\". Keep ids as text: as numbers they lose digits.",
        call. = FALSE
      )
    }
  }
  if (!is.null(target_username)) {
    ok <- is.character(target_username) && length(target_username) == 1 &&
      !is.na(target_username) && nzchar(target_username)
    if (!ok) {
      stop(
        "`target_username` must be one handle, such as \"XDevelopers\".",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

# The other account's id: as given, or looked up from the handle.
resolve_target_user <- function(target_username, target_user_id, token) {
  if (!is.null(target_user_id)) {
    return(target_user_id)
  }
  lookup_user_id(sub("^@", "", target_username), token$access_token)
}
