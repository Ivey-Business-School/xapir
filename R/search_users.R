#' Search Users
#'
#' @description
#' Finds accounts whose name, handle or bio match a search string via the
#' [search users endpoint](https://docs.x.com/x-api/users/search-users).
#' Needs a user token, so the first call opens a browser window to sign in.
#' Every user returned is billed, so the function says what the call can
#' cost before it reads anything, and stops reading at `max_users`.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr pluck
#' @param query One search string, such as "electric vehicles".
#' @param max_results \code{numeric}; the number of users per API call,
#'   between 1 and 1000. The function stops before any request if the value
#'   is outside that range.
#' @param max_users \code{numeric}; the most users to read across all pages.
#'   Reading stops once this many have been returned. Default 100.
#' @template user_fields
#' @return A tibble with one row per user and the 24 columns described in
#'   [extract_user()], from `created_at` to `user_id`. When nothing matches,
#'   the same columns with no rows.
#' @examples
#' \dontrun{
#' ev_accounts <- search_users("electric vehicles", max_users = 50)
#' }
#' @export
search_users <- function(
  query,
  max_results = 100,
  max_users   = 100,
  user_fields = default_user_fields()
) {
  check_query(query)
  check_max_results(max_results, min = 1, max = 1000, what = "users")
  check_max_users(max_users)
  announce_cap(max_users, what = "users")

  token <- authenticate_user()

  req <- x_request(token$access_token) |>
    req_url_path_append("users", "search") |>
    req_url_query(query = query, user.fields = join_fields(user_fields))

  # This endpoint names its page token `next_token`, not `pagination_token`
  # as the others do, so it walks its own pages.
  users      <- list()
  n          <- 0
  next_token <- NULL
  page_i     <- 1

  repeat {
    remaining <- max_users - n

    page <- req |>
      req_url_query(
        max_results = max(min(max_results, remaining), 1),
        next_token  = next_token
      ) |>
      x_perform()

    found <- page$data %||% list()
    if (length(found) > remaining) {
      found <- found[seq_len(remaining)]
    }
    users <- c(users, found)
    n     <- n + length(found)
    message("Finished getting users on page ", page_i)

    next_token <- pluck(page, "meta", "next_token")
    if (is.null(next_token) || n >= max_users || length(found) == 0) {
      break
    }
    page_i <- page_i + 1
  }

  announce_total(n, what = "users")
  users_table(users)
}
