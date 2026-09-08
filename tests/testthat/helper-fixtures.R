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
