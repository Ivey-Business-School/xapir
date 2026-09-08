## Internal helpers shared by every function that calls the X API.
## Nothing in this file is exported.

#' @importFrom httr2 request req_auth_bearer_token req_user_agent
#'   req_url_path_append req_url_query req_retry req_error req_perform
#'   resp_body_json resp_status resp_header
NULL

# Field defaults -------------------------------------------------------------

# The same five field sets and one expansion set serve every posts endpoint.
# Change them here and every reader follows.

default_post_fields <- function() {
  c("created_at", "text", "note_tweet", "article", "public_metrics", "geo",
    "attachments", "context_annotations", "entities", "lang",
    "possibly_sensitive", "edit_controls", "referenced_tweets",
    "reply_settings", "conversation_id", "in_reply_to_user_id", "author_id",
    "edit_history_tweet_ids", "id")
}

default_user_fields <- function() {
  c("created_at", "description", "protected", "entities", "location",
    "profile_image_url", "public_metrics", "verified", "verified_type",
    "is_identity_verified", "url")
}

default_media_fields <- function() {
  c("duration_ms", "height", "width", "preview_image_url", "type", "url",
    "alt_text", "public_metrics", "variants", "media_key")
}

default_poll_fields <- function() {
  c("end_datetime", "duration_minutes", "options", "voting_status", "id")
}

default_place_fields <- function() {
  c("country", "country_code", "full_name", "geo", "id", "place_type")
}

default_expansions <- function() {
  c("author_id", "entities.mentions.username",
    "referenced_tweets.id.author_id", "referenced_tweets.id",
    "in_reply_to_user_id", "attachments.media_keys", "attachments.poll_ids",
    "geo.place_id")
}

# The API wants each field set as one comma-separated string.
join_fields <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  str_c(x, collapse = ",")
}

# Builds the query list a posts endpoint expects from the six field vectors.
field_query <- function(post_fields, user_fields, media_fields, poll_fields,
                        place_fields, expansions) {
  list(
    tweet.fields = join_fields(post_fields),
    user.fields  = join_fields(user_fields),
    media.fields = join_fields(media_fields),
    poll.fields  = join_fields(poll_fields),
    place.fields = join_fields(place_fields),
    expansions   = join_fields(expansions)
  )
}

# Guardrails -----------------------------------------------------------------

# The price X charges per post returned, in US dollars, as of September 2026.
x_price_per_post <- 0.005

check_max_results <- function(max_results) {
  ok <- is.numeric(max_results) && length(max_results) == 1 &&
    !is.na(max_results) && max_results >= 10 && max_results <= 100
  if (!ok) {
    stop(
      "`max_results` must be a number between 10 and 100. ",
      "The X API returns at least 10 and at most 100 posts a page.",
      call. = FALSE
    )
  }
  invisible(as.integer(max_results))
}

check_max_posts <- function(max_posts) {
  ok <- is.numeric(max_posts) && length(max_posts) == 1 &&
    !is.na(max_posts) && max_posts >= 1
  if (!ok) {
    stop("`max_posts` must be a number of 1 or more.", call. = FALSE)
  }
  invisible(max_posts)
}

# One line, before the first request, so the reader knows what the call can
# cost. Every post returned is billed, so the cap is the worst case.
announce_cap <- function(max_posts, price = x_price_per_post) {
  message(sprintf(
    "Reading up to %s posts, about $%.2f. Set max_posts to change this.",
    format(max_posts, big.mark = ",", scientific = FALSE),
    max_posts * price
  ))
}

# Requests -------------------------------------------------------------------

# Stops before anything is announced or requested when the token is empty.
check_token <- function(token) {
  if (is.null(token) || length(token) != 1 || is.na(token) || !nzchar(token)) {
    stop(
      "No token. Put X_BEARER_TOKEN=<your token> in your .Renviron file, ",
      "restart R, and try again.",
      call. = FALSE
    )
  }
  invisible(token)
}

# The base request every reader starts from.
x_request <- function(token) {
  check_token(token)
  request("https://api.x.com/2") |>
    req_auth_bearer_token(token) |>
    req_user_agent("xapir (https://github.com/Ivey-Business-School/xapir)")
}

# Performs a request and parses the JSON body.
#
# Retries only when the API says to wait: a 429 (rate limit) or a 5xx (their
# side). It waits as long as x-rate-limit-reset or Retry-After asks, and gives
# up after max_tries attempts. Any other 4xx (a bad token, a misspelled handle,
# a field the tier cannot see) stops at once with the API's own message.
x_perform <- function(req, max_tries = 4) {
  req |>
    req_retry(
      max_tries        = max_tries,
      is_transient     = x_is_transient,
      after            = x_retry_after,
      retry_on_failure = FALSE
    ) |>
    req_error(body = x_error_body) |>
    req_perform() |>
    resp_body_json()
}

x_is_transient <- function(resp) {
  status <- resp_status(resp)
  status == 429 || (status >= 500 && status < 600)
}

# Seconds to wait before the next try, or NA to let httr2 back off.
x_retry_after <- function(resp) {
  retry_after <- resp_header(resp, "Retry-After")
  if (!is.null(retry_after)) {
    return(max(as.numeric(retry_after), 0))
  }
  reset <- resp_header(resp, "x-rate-limit-reset")
  if (!is.null(reset)) {
    return(max(as.numeric(reset) - as.numeric(Sys.time()), 1))
  }
  NA
}

# The API's own words, pulled from the error body for the message.
x_error_body <- function(resp) {
  body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  if (is.null(body)) return(NULL)
  from_errors <- unlist(lapply(body$errors, function(e) {
    e$message %||% e$detail %||% e$title
  }))
  parts <- c(body$title, body$detail, from_errors)
  parts <- unique(as.character(parts[!vapply(parts, is.null, TRUE)]))
  if (length(parts) == 0) NULL else parts
}

# Turns a username into a user id, or stops with the API's reason.
lookup_user_id <- function(username, token) {
  body <- x_request(token) |>
    req_url_path_append("users", "by", "username", username) |>
    x_perform()

  user_id <- pluck(body, "data", "id")
  if (is.null(user_id)) {
    reason <- unlist(lapply(body$errors, function(e) e$detail %||% e$title))
    stop(
      "No user found for username \"", username, "\". ",
      paste(reason, collapse = " "),
      call. = FALSE
    )
  }
  user_id
}

# Pages -----------------------------------------------------------------------

# Walks a paginated posts endpoint until next_token runs out or max_posts is
# reached. Returns the list of pages the extractors read. The last page is
# trimmed so the pull never holds more than max_posts posts.
fetch_pages <- function(req, max_posts, max_results = 100, sleep_time = 0,
                        pagination_token = NULL, what = "posts") {
  response     <- list()
  post_counter <- 0
  call_i       <- 1

  repeat {
    remaining <- max_posts - post_counter

    page <- req |>
      req_url_query(
        max_results      = max(min(max_results, remaining), 10),
        pagination_token = pagination_token
      ) |>
      x_perform()

    n <- length(page$data)
    if (n > remaining) {
      page$data <- page$data[seq_len(remaining)]
      n <- remaining
    }

    response     <- c(response, list(page))
    post_counter <- post_counter + n
    message("Finished getting ", what, " on page ", call_i)

    pagination_token <- pluck(page, "meta", "next_token")
    if (is.null(pagination_token) || post_counter >= max_posts || n == 0) {
      break
    }

    call_i <- call_i + 1
    Sys.sleep(sleep_time)
  }

  response
}
