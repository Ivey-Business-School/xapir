#' Get Recent Post Count
#'
#' @description
#' Returns Post Counts from the last 7 days that match a search query via the [recent posts count 
#' endpoint](https://docs.x.com/x-api/posts/recent-search-counts).
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_path_append req_perform resp_body_json req_url_query
#' @importFrom purrr pluck
#' @importFrom stringr str_c
#' @param query The search to be made on X. You can find ways to build specific queries according to the [X API documentation website](https://docs.x.com/x-api/posts/search/integrate/build-a-query#types)
#' @param start_time The earliest date-time from which you want to get posts.
#' @param end_time The latest date-time from which you want to get posts.
#' Provide the value in ISO 8601 format (i.e., `YYYY-MM-DDTHH:mm:ssZ`). The
#'   `iso_8601()` function will convert a string, date, or date-time object to
#'   the required format (e.g., `iso_8601("2024-10-10")`).
#' @param granularity The granularity for the search counts results. This takes either the value 'minute', 'hour', or 'day'.
#' @template bearer_token
#' @return A tibble containing the number of posts.
#' @examples
#' \dontrun{
#' tl <- get_recent_post_count("Developers")
#' }
#' @export
get_recent_post_count <- function(
    query,
    start_time       = NULL,
    end_time         = NULL,
    granularity      = "hour",
    bearer_token     = Sys.getenv("X_BEARER_TOKEN")
) {

  # Make the API request
  while (TRUE) {
    expr = {
        request(base_url = "https://api.x.com/2") |>
        req_url_path_append(
            endpoint = paste0("tweets/counts/recent")
        ) |>
        req_url_query(
            query            = query,
            end_time         = end_time,
            start_time       = start_time
        ) |>
        req_auth_bearer_token(token = bearer_token) |>
        req_perform() |>
        resp_body_json() ->
        this_response

        # Exit the loop if successful
        break
    }
 }

counts <- pluck(this_response, "meta", .default = NULL)

if (is.null(counts)) {
    message("No count data found.")
    return(NULL)
}

counts <- as.data.frame(counts)

# Return the response
return(counts)
}
