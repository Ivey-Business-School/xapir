#' Get Liking Users
#'
#' @description
#' Retrieves a list of Users who liked a specific Post by its ID via the [get
#' liking users endpoint](https://docs.x.com/x-api/posts/get-liking-users).
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_path_append req_perform resp_body_json req_url_query
#' @importFrom purrr pluck
#' @importFrom stringr str_c
#' @param post_id The ID of the Post whose liking Users are to be retrieved.
#' @template max_results
#' @template pagination_token
#' @param sleep_time A numeric value specifying the number of seconds to wait
#'   between API calls. This helps avoid hitting rate limits imposed by the X
#'   API. You can adjust this value based on your tier's rate limits, which are
#'   detailed on the [X API documentation
#'   website](https://developer.x.com/en/docs/rate-limits).
#' @template user_fields
#' @return A \code{list} containing the API response(s)
#' @examples
#' \dontrun{
#' users <- get_liking_users(post_id = "1234567890")
#' }
#' @export
get_liking_users <- function(
    post_id,
    max_results      = 100,
    pagination_token = NULL,
    sleep_time       = 90,
    user_fields      =
      c("created_at", "description", "protected", "entities", "location",
        "profile_image_url", "public_metrics", "verified", "verified_type")
) {

  # Get cached or refreshed token
  token <- authenticate_user()

  response <- NULL

  # Join the fields with commas as the API expects
  user_fields_str <- str_c(user_fields, collapse = ",")

  call_i <- 1

  # Make the API request
  while ((call_i == 1 || !is.null(pagination_token))) {

    while (TRUE) {
      tryCatch(
        expr = {
          request(base_url = "https://api.x.com/2") |>
            req_url_path_append(
              endpoint = paste0("tweets/", post_id, "/liking_users")
            ) |>
            req_url_query(
              max_results      = max_results,
              pagination_token = pagination_token,
              user.fields      = user_fields_str
            ) |>
            req_auth_bearer_token(token = token$access_token) |>
            req_perform() |>
            resp_body_json() ->
            this_response

          # Exit the loop if successful
          break
        },
        error = function(e) {
          message(e$message, " Retrying in 60 seconds.")
          Sys.sleep(60)
        }
      )
    }

    response <- c(response, list(this_response))

    this_response |>
      pluck("meta", "next_token") ->
      pagination_token

    message(paste("Finished getting users on page ", call_i))

    call_i <- call_i + 1

    # Sleep time between API requests
    Sys.sleep(sleep_time)
  }

  # Return the response
  return(response)
}
