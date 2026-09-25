# The first two pages of a Tesla timeline pulled on 8 September 2026 with the
# package's default fields. Public data, about 100 KB. Nothing here calls the
# API.
tesla_pages <- function() {
  readRDS(test_path("fixtures", "tesla-2-pages.rds"))
}

# One hand-built page: post 1 quotes post 2, which lives only in includes and
# carries a poll. Used where the Tesla pages have no example.
poll_page <- function() {
  list(
    data = list(
      list(
        id = "1", author_id = "u1", text = "quoting", created_at = "2026-09-01T10:00:00.000Z",
        referenced_tweets = list(list(type = "quoted", id = "2"))
      )
    ),
    includes = list(
      tweets = list(
        list(
          id = "2", author_id = "u2", text = "poll?", created_at = "2026-09-01T09:00:00.000Z",
          attachments = list(poll_ids = list("p1"))
        )
      ),
      polls = list(
        list(
          id = "p1", duration_minutes = 60L, end_datetime = "2026-09-01T10:00:00.000Z",
          voting_status = "closed",
          options = list(
            list(position = 1L, label = "Yes", votes = 3L),
            list(position = 2L, label = "No", votes = 1L)
          )
        )
      ),
      users = list(
        list(id = "u1", username = "one"),
        list(id = "u2", username = "two")
      )
    ),
    meta = list(result_count = 1L)
  )
}

# A JSON response body for httr2 mocks.
json_body <- function(x) {
  charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE))
}

json_response <- function(status, body, headers = list()) {
  httr2::response(
    status_code = status,
    headers = c(list(`content-type` = "application/json"), headers),
    body = json_body(body)
  )
}

# A page of n posts with ids from `from`, and a next_token when given.
posts_page <- function(ids, next_token = NULL) {
  meta <- list(result_count = length(ids))
  if (!is.null(next_token)) meta$next_token <- next_token
  json_response(200, list(
    data = lapply(ids, function(i) list(id = as.character(i), text = "t")),
    meta = meta
  ))
}

# Shared by several test files ---------------------------------------------
 # A fake user token, so no test opens a browser.

fake_token <- function(...) list(access_token = "tok")

record_requests <- function(respond) {
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1]] <<- req
    respond(req)
  }, env = parent.frame())
  function() seen
}

sent_json <- function(req) {
  data <- req$body$data
  if (is.character(data)) {
    jsonlite::fromJSON(data, simplifyVector = FALSE)
  } else {
    data
  }
}

api_user <- function(id, username = paste0("user", id), url = "https://t.co/x") {
  list(
    id = as.character(id),
    username = username,
    name = paste("User", id),
    description = "bio",
    created_at = "2020-01-02T03:04:05.000Z",
    protected = FALSE,
    verified = TRUE,
    verified_type = "blue",
    is_identity_verified = FALSE,
    location = "Toronto",
    profile_image_url = "https://pbs.twimg.com/x.jpg",
    profile_banner_url = "https://pbs.twimg.com/banner.jpg",
    verified_followers_count = 3L,
    subscription_type = "Premium",
    parody = FALSE,
    pinned_tweet_id = "555",
    url = url,
    entities = list(url = list(urls = list(list(display_url = "example.com")))),
    public_metrics = list(
      followers_count = 10L, following_count = 5L, tweet_count = 100L,
      listed_count = 2L, like_count = 7L, media_count = 40L
    )
  )
}

users_page <- function(ids, next_token = NULL) {
  meta <- list(result_count = length(ids))
  if (!is.null(next_token)) meta$next_token <- next_token
  json_response(200, list(data = lapply(ids, api_user), meta = meta))
}

mock_user_token <- function(env = parent.frame()) {
  .x_env$my_user_id <- NULL
  testthat::local_mocked_bindings(
    authenticate_user = function(...) list(access_token = "tok"),
    .package = "xapir",
    .env = env
  )
}

collect_messages <- function(expr) {
  msgs <- character(0)
  result <- withCallingHandlers(
    expr,
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(result = result, msgs = trimws(msgs))
}

count_page <- function(rows) {
  json_response(200, list(
    data = rows,
    meta = list(total_tweet_count = sum(vapply(rows, function(r) r$tweet_count, 1L)))
  ))
}

# The Authorization header of a request, whichever httr2 is installed.
# httr2 1.2.0 started storing redacted headers behind a sentinel that
# req_get_headers() reveals; older versions keep the plain string.
auth_header <- function(req) {
  if (exists("req_get_headers", envir = asNamespace("httr2"), inherits = FALSE)) {
    httr2::req_get_headers(req, redacted = "reveal")[["Authorization"]]
  } else {
    req$headers$Authorization
  }
}
