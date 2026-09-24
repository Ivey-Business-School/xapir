## Internal helpers shared by every function that calls the X API.
## Nothing in this file is exported.

#' @importFrom httr2 request req_auth_bearer_token req_user_agent
#'   req_url_path_append req_url_query req_retry req_error req_perform
#'   resp_body_json resp_status resp_header
NULL

# Field defaults -------------------------------------------------------------

# The same five field sets and one expansion set serve every posts endpoint.
# Change them here and every reader follows. The names are the ones the live
# API still answers with (note_tweet, referenced_tweets, tweet.fields), even
# where the OpenAPI spec has renamed them to "post".

# Everything the post tables read. `community_id` is set on a post made in
# an X community and `paid_partnership` is TRUE when the author disclosed
# the post as paid promotion. The spec also lists display_text_range,
# card_uri, source, withheld, scopes, media_metadata and article_title;
# none of those fills a table column, so they are not asked for.
default_post_fields <- function() {
  c(
    "created_at", "text", "note_tweet", "article", "public_metrics", "geo",
    "attachments", "context_annotations", "entities", "lang",
    "possibly_sensitive", "edit_controls", "referenced_tweets",
    "reply_settings", "conversation_id", "in_reply_to_user_id", "author_id",
    "edit_history_tweet_ids", "community_id", "paid_partnership", "id"
  )
}

# Every public user field the user table reads. The spec's other user
# fields (connection_status, confirmed_email, receives_your_dm,
# subscribes_to_you) describe the relationship with the signed-in user, so
# an app bearer token cannot request them and they are left out. `withheld`
# and `subscriber_count` are not read by the table.
default_user_fields <- function() {
  c(
    "created_at", "description", "protected", "entities", "location",
    "profile_image_url", "profile_banner_url", "public_metrics", "verified",
    "verified_type", "verified_followers_count", "subscription_type",
    "parody", "is_identity_verified", "url"
  )
}

default_media_fields <- function() {
  c(
    "duration_ms", "height", "width", "preview_image_url", "type", "url",
    "alt_text", "public_metrics", "variants", "media_key"
  )
}

default_poll_fields <- function() {
  c("end_datetime", "duration_minutes", "options", "voting_status", "id")
}

default_place_fields <- function() {
  c("country", "country_code", "full_name", "geo", "id", "place_type")
}

# The related objects the tables read. The API also offers
# attachments.media_source_tweet, article.cover_media, article.media_entities
# and edit_history_tweet_ids; each adds includes no table reads, so they are
# not asked for by default but can be passed through `expansions`.
default_expansions <- function() {
  c(
    "author_id", "entities.mentions.username",
    "referenced_tweets.id.author_id", "referenced_tweets.id",
    "in_reply_to_user_id", "attachments.media_keys", "attachments.poll_ids",
    "geo.place_id"
  )
}

# The API wants each field set as one comma-separated string.
join_fields <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
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

# Prices, in US dollars, from docs.x.com/x-api/getting-started/pricing on
# 24 September 2026. Reads are billed per item returned; the rest are billed
# per request. The course can move any of them without a release:
# options(xapir.prices = list(posts = 0.006)) in .Rprofile overrides one
# entry and every cost message follows. The older options
# xapir.price_per_post and xapir.price_per_user still work.
x_default_prices <- list(
  # per item returned
  posts       = 0.005,
  users       = 0.010,
  follows     = 0.010,   # followers and following
  lists       = 0.005,
  likes       = 0.001,   # who liked a post
  mutes       = 0.001,
  blocks      = 0.001,
  spaces      = 0.005,
  communities = 0.005,
  # per request
  counts_recent = 0.005,
  counts_all    = 0.010,
  trends        = 0.010,
  post_create   = 0.015,
  post_create_with_url = 0.200,
  interaction   = 0.015,   # like, follow, repost, block, mute
  interaction_delete = 0.010,
  content_manage = 0.005,  # delete a post, hide a reply
  list_create   = 0.010,
  list_manage   = 0.005,
  bookmark      = 0.005,
  media_metadata = 0.005
)
x_price_per_post <- x_default_prices$posts

# Dollars as text, rounding a half cent up. C's printf rounds a half cent
# differently on Windows and Linux, and R's round() sends it to zero, so a
# $0.005 request would read as free. Half up is also the worst case, which
# is what a cost line should show.
dollars <- function(x, digits = 2) {
  m <- 10^digits
  sprintf(paste0("%.", digits, "f"), floor(x * m + 0.5 + 1e-8) / m)
}

# The current price of one item, or one request, of `what`.
x_price <- function(what = "posts") {
  prices <- utils::modifyList(x_default_prices, getOption("xapir.prices", list()))
  if (identical(what, "posts")) {
    return(getOption("xapir.price_per_post", prices$posts))
  }
  if (identical(what, "users")) {
    return(getOption("xapir.price_per_user", prices$users))
  }
  price <- prices[[what]]
  if (is.null(price)) {
    stop("No price is known for \"", what, "\".", call. = FALSE)
  }
  price
}

# The noun the cost line uses for `what`. Followers are users, likes are
# users who liked, and so on.
x_unit <- function(what) {
  switch(what,
    posts = "posts", users = "users", follows = "users", likes = "users",
    mutes = "users", blocks = "users", lists = "lists", spaces = "spaces",
    communities = "communities", what
  )
}

# Each endpoint has its own page-size range: posts endpoints take 10 to 100,
# followers up to 1,000, full-archive search up to 500. The reader passes
# the range from the API reference and the message names it.
check_max_results <- function(max_results, min = 10, max = 100, what = "posts") {
  ok <- is.numeric(max_results) && length(max_results) == 1 &&
    !is.na(max_results) && max_results >= min && max_results <= max
  if (!ok) {
    stop(
      "`max_results` must be a number between ", min, " and ", max, ". ",
      "The X API returns at least ", min, " and at most ", max, " ", what,
      " a page.",
      call. = FALSE
    )
  }
  invisible(as.integer(max_results))
}

# A finite cap of 1 or more. Inf is refused: it would announce "up to Inf
# posts" and then read until the API or the budget ran out.
check_max_posts <- function(max_posts, arg = "max_posts") {
  ok <- is.numeric(max_posts) && length(max_posts) == 1 &&
    is.finite(max_posts) && max_posts >= 1
  if (!ok) {
    stop(
      "`", arg, "` must be a finite number of 1 or more, such as 500. ",
      "Every item returned is billed, so there is no unlimited pull.",
      call. = FALSE
    )
  }
  invisible(max_posts)
}

# The same rule for readers that return users and bill per user.
check_max_users <- function(max_users) {
  check_max_posts(max_users, arg = "max_users")
}

# Ids travel as text: as numbers they lose digits. Stops before any request
# when an id is not a string of digits or there are more than the endpoint
# takes in one call.
check_post_ids <- function(post_ids, max_ids = 100, arg = "post_ids") {
  ok <- is.character(post_ids) && length(post_ids) >= 1 &&
    !anyNA(post_ids) && all(grepl("^[0-9]+$", post_ids))
  if (!ok) {
    stop(
      "`", arg, "` must be ", if (max_ids == 1) "one string" else "strings",
      " of digits, such as \"1234567890123456789\". ",
      "Keep ids as text: as numbers they lose digits.",
      call. = FALSE
    )
  }
  if (length(post_ids) > max_ids) {
    stop(
      "`", arg, "` can hold at most ", max_ids,
      if (max_ids == 1) " id" else " ids", " per call.",
      call. = FALSE
    )
  }
  invisible(post_ids)
}

# One line, before the first request, so the reader knows what the call can
# cost. Every item returned is billed, so the cap is the worst case. `what` is
# "posts" or "users" and picks the price; `price` overrides it; `arg` names
# the argument that moves the cap.
announce_cap <- function(max_posts, price = NULL, what = "posts", arg = NULL) {
  price <- price %||% x_price(what)
  unit <- x_unit(what)
  arg <- arg %||% if (identical(unit, "users")) "max_users" else "max_posts"
  message(sprintf(
    "Reading up to %s %s, about $%s. Set %s to change this.",
    format(max_posts, big.mark = ",", scientific = FALSE),
    unit, dollars(max_posts * price), arg
  ))
}

# One line for an endpoint billed per request rather than per item: counts,
# trends and every write.
announce_request_cost <- function(what, n = 1) {
  price <- x_price(what)
  if (n == 1) {
    message(sprintf("This request costs about $%s.", dollars(price, 3)))
  } else {
    message(sprintf(
      "%s requests, about $%s in total.",
      format(n, big.mark = ","), dollars(n * price)
    ))
  }
}

# One line after the last page, so a run that stopped early shows what it
# actually spent.
announce_total <- function(n, what = "posts", price = NULL) {
  price <- price %||% x_price(what)
  message(sprintf(
    "Read %s %s, about $%s.",
    format(n, big.mark = ",", scientific = FALSE), x_unit(what), dollars(n * price)
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

# Exactly one of username and user_id, so a call never pays for a lookup it
# did not ask for and never addresses the wrong account.
check_one_of_user <- function(username, user_id) {
  if (is.null(username) == is.null(user_id)) {
    stop(
      "Give either `username` or `user_id`, not both and not neither.",
      call. = FALSE
    )
  }
  if (!is.null(user_id)) {
    ok <- is.character(user_id) && length(user_id) == 1 &&
      !is.na(user_id) && grepl("^[0-9]+$", user_id)
    if (!ok) {
      stop(
        "`user_id` must be one string of digits, such as \"2244994945\". ",
        "Keep ids as text: as numbers they lose digits.",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
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
  if (is.null(body)) {
    return(NULL)
  }
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
# trimmed so the pull never holds more than max_posts posts, and the total
# read and its cost are printed at the end. A pause between pages is
# optional: x_perform() already waits as long as a 429 asks.
fetch_pages <- function(req, max_posts, max_results = 100, sleep_time = 0,
                        pagination_token = NULL, what = "posts",
                        min_results = 10) {
  response <- list()
  post_counter <- 0
  call_i <- 1

  repeat {
    remaining <- max_posts - post_counter

    page <- req |>
      req_url_query(
        max_results      = max(min(max_results, remaining), min_results),
        pagination_token = pagination_token
      ) |>
      x_perform()

    n <- length(page$data)
    if (n > remaining) {
      page$data <- page$data[seq_len(remaining)]
      n <- remaining
    }

    response <- c(response, list(page))
    post_counter <- post_counter + n
    message("Finished getting ", x_unit(what), " on page ", call_i)

    pagination_token <- pluck(page, "meta", "next_token")
    if (is.null(pagination_token) || post_counter >= max_posts || n == 0) {
      break
    }

    call_i <- call_i + 1
    Sys.sleep(sleep_time)
  }

  announce_total(post_counter, what = what)
  response
}

# Searches -------------------------------------------------------------------

# One non-empty search string, so a blank query never reaches the API.
check_query <- function(query) {
  ok <- is.character(query) && length(query) == 1 && !is.na(query) &&
    nzchar(trimws(query))
  if (!ok) {
    stop(
      "`query` must be one search string, such as \"#marketing lang:en\".",
      call. = FALSE
    )
  }
  invisible(query)
}

# The three period sizes the counts endpoint takes.
check_granularity <- function(granularity) {
  choices <- c("minute", "hour", "day")
  ok <- is.character(granularity) && length(granularity) == 1 &&
    !is.na(granularity) && granularity %in% choices
  if (!ok) {
    stop(
      "`granularity` must be \"minute\", \"hour\" or \"day\".",
      call. = FALSE
    )
  }
  invisible(granularity)
}
