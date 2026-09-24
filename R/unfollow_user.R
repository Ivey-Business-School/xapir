#' Unfollow User
#'
#' @description
#' Makes the source account unfollow the target account via the [unfollow
#' user endpoint](https://docs.x.com/x-api/users/unfollow-user). The source
#' must be the account that signed in. Needs a user token, so the first call
#' opens a browser window to sign in.
#'
#' @importFrom httr2 req_method
#' @param source_username Username of the account that will unfollow, without
#'   the "@" symbol. Must be the account that signed in.
#' @param target_username Username of the account to unfollow, without the
#'   "@" symbol.
#' @return Invisibly, the `data` list the API returns, `list(following =
#'   FALSE)`. Stops with the API's message when the request is refused, for
#'   example when the source does not follow the target.
#' @examples
#' \dontrun{
#' unfollow_user(source_username = "Tesla", target_username = "elonmusk")
#' }
#' @export
unfollow_user <- function(
  source_username,
  target_username
) {

  token          <- authenticate_user()
  source_user_id <- lookup_user_id(source_username, token$access_token)
  target_user_id <- lookup_user_id(target_username, token$access_token)

  announce_request_cost("interaction_delete")

  response <- x_request(token$access_token) |>
    req_url_path_append("users", source_user_id, "following", target_user_id) |>
    req_method("DELETE") |>
    x_perform()

  invisible(response$data)
}
