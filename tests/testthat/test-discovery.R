# Spaces, communities and news. Every test here mocks the API. Nothing
# calls X.

discovery_messages <- function(expr) {
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

# One request that must never happen. A validation test installs this so a
# stop that came too late shows up as "a request was made".
no_request <- function() {
  httr2::local_mocked_responses(function(req) stop("a request was made"),
                                env = parent.frame())
}

# Objects as the API sends them ---------------------------------------------

api_space <- function(id, title = paste("Space", id)) {
  list(
    id = id, title = title, state = "live", creator_id = "42",
    created_at = "2026-09-01T10:00:00.000Z",
    scheduled_start = "2026-09-01T11:00:00.000Z",
    started_at = "2026-09-01T11:02:00.000Z",
    lang = "en", is_ticketed = FALSE, participant_count = 120L,
    subscriber_count = 7L, host_ids = list("42", "43"), speaker_ids = list("44")
  )
}

api_community <- function(id, name = paste("Community", id)) {
  list(
    id = as.character(id), name = name, description = "about",
    access = "Public", join_policy = "Open", member_count = 1500L,
    created_at = "2022-02-15T10:00:00.000Z"
  )
}

api_news <- function(id) {
  list(
    id = as.character(id), name = "Nebius Group Stock Plunges 30%",
    hook = "Shares cratered.", summary = "A long summary.", category = "News",
    disclaimer = "Grok can make mistakes.",
    keywords = list("Nebius", "stocks"),
    contexts = list(
      sports = list(teams = list()),
      entities = list(
        events = list(), organizations = list("Goldman Sachs", "Nebius Group N.V."),
        people = list(), places = list(), products = list()
      ),
      topics = list("Stocks"),
      finance = list(tickers = list("NBIS"))
    ),
    last_updated_at_ms = "2025-11-17T16:21:41.000Z"
  )
}

space_columns <- c(
  "space_id", "title", "state", "creator_id", "created_at", "scheduled_start",
  "started_at", "ended_at", "lang", "is_ticketed", "participant_count",
  "subscriber_count", "host_ids", "speaker_ids"
)
community_columns <- c(
  "community_id", "name", "description", "access", "join_policy",
  "member_count", "created_at"
)
news_columns <- c(
  "news_id", "name", "hook", "summary", "category", "disclaimer", "keywords",
  "contexts", "updated_at"
)

# Schemas ---------------------------------------------------------------------

test_that("the discovery schemas, tables and rows agree on columns and types", {
  expect_equal(names(space_schema()), space_columns)
  expect_identical(spaces_table(list()), space_schema())
  expect_identical(spaces_table(NULL), space_schema())
  row <- space_row(list())
  expect_equal(names(row), space_columns)
  expect_s3_class(row$created_at, "POSIXct")
  expect_type(row$participant_count, "integer")
  expect_type(row$is_ticketed, "logical")
  expect_identical(row$host_ids[[1]], character(0))

  expect_equal(names(community_schema()), community_columns)
  expect_identical(communities_table(list()), community_schema())
  row <- community_row(list())
  expect_equal(names(row), community_columns)
  expect_true(all(vapply(row, function(col) is.na(col), TRUE)))
  expect_type(row$member_count, "integer")

  expect_equal(names(news_schema()), news_columns)
  expect_identical(news_table(list()), news_schema())
  row <- news_row(list())
  expect_equal(names(row), news_columns)
  expect_identical(row$keywords[[1]], character(0))
  expect_identical(row$contexts[[1]], character(0))
  expect_s3_class(row$updated_at, "POSIXct")
})

# Spaces ---------------------------------------------------------------------

test_that("get_spaces by space_ids hits /2/spaces and returns the space table", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(data = list(api_space("1DXxyRYNejbKM"), api_space("1nAKErYNqlpxL"))))
  })
  out <- discovery_messages(
    get_spaces(space_ids = c("1DXxyRYNejbKM", "1nAKErYNqlpxL"), bearer_token = "tok")
  )
  expect_match(out$msgs[1], "Reading up to 2 spaces, about \\$0.01. Set space_ids")
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/spaces?", fixed = TRUE)
  expect_match(urls[1], "ids=1DXxyRYNejbKM%2C1nAKErYNqlpxL", fixed = TRUE)
  expect_match(urls[1], "space.fields=created_at%2Cended_at%2Cid", fixed = TRUE)

  spaces <- out$result
  expect_equal(names(spaces), space_columns)
  expect_equal(spaces$space_id, c("1DXxyRYNejbKM", "1nAKErYNqlpxL"))
  expect_equal(spaces$state, c("live", "live"))
  expect_equal(spaces$creator_id, c("42", "42"))
  expect_s3_class(spaces$created_at, "POSIXct")
  expect_equal(attr(spaces$created_at, "tzone"), "UTC")
  expect_s3_class(spaces$scheduled_start, "POSIXct")
  expect_s3_class(spaces$started_at, "POSIXct")
  expect_true(all(is.na(spaces$ended_at)))
  expect_equal(spaces$lang, c("en", "en"))
  expect_equal(spaces$is_ticketed, c(FALSE, FALSE))
  expect_equal(spaces$participant_count, c(120L, 120L))
  expect_equal(spaces$subscriber_count, c(7L, 7L))
  expect_type(spaces$host_ids, "list")
  expect_equal(spaces$host_ids[[1]], c("42", "43"))
  expect_equal(spaces$speaker_ids[[2]], "44")
})

test_that("get_spaces by user_ids hits /2/spaces/by/creator_ids and warns on partial errors", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(
      data = list(api_space("1DXxyRYNejbKM")),
      errors = list(list(title = "Not Found Error", detail = "Could not find user with id: [999]."))
    ))
  })
  expect_warning(
    spaces <- suppressMessages(get_spaces(user_ids = c("42", "999"), bearer_token = "tok")),
    "1 of the user ids could not be read. Could not find user with id"
  )
  expect_match(urls[1], "/2/spaces/by/creator_ids?", fixed = TRUE)
  expect_match(urls[1], "user_ids=42%2C999", fixed = TRUE)
  expect_equal(nrow(spaces), 1)
  expect_equal(spaces$space_id, "1DXxyRYNejbKM")
})

test_that("get_spaces returns the zero-row schema when nothing is found", {
  httr2::local_mocked_responses(list(
    json_response(200, list(meta = list(result_count = 0L)))
  ))
  spaces <- suppressMessages(get_spaces(space_ids = "1DXxyRYNejbKM", bearer_token = "tok"))
  expect_identical(spaces, space_schema())
})

test_that("get_spaces validates before any request", {
  no_request()
  expect_error(get_spaces(bearer_token = "tok"), "either `space_ids` or `user_ids`")
  expect_error(
    get_spaces(space_ids = "1DXxyRYNejbKM", user_ids = "42", bearer_token = "tok"),
    "either `space_ids` or `user_ids`"
  )
  expect_error(
    get_spaces(space_ids = paste0("s", 1:101), bearer_token = "tok"),
    "at most 100 ids"
  )
  expect_error(get_spaces(space_ids = "has space!", bearer_token = "tok"), "letters and digits")
  expect_error(get_spaces(space_ids = 123, bearer_token = "tok"), "letters and digits")
  expect_error(
    get_spaces(user_ids = as.character(1:101), bearer_token = "tok"),
    "at most 100 ids"
  )
  expect_error(get_spaces(user_ids = 42, bearer_token = "tok"), "string of digits")
  expect_error(get_spaces(space_ids = "1DXxyRYNejbKM", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("search_spaces hits /2/spaces/search with query, state and max_results", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(data = list(api_space("1DXxyRYNejbKM")), meta = list(result_count = 1L)))
  })
  out <- discovery_messages(
    search_spaces("marketing", state = "live", max_results = 20, bearer_token = "tok")
  )
  expect_match(out$msgs[1], "Reading up to 20 spaces, about \\$0.10. Set max_results")
  expect_match(urls[1], "/2/spaces/search?", fixed = TRUE)
  expect_match(urls[1], "query=marketing", fixed = TRUE)
  expect_match(urls[1], "state=live", fixed = TRUE)
  expect_match(urls[1], "max_results=20", fixed = TRUE)
  expect_equal(names(out$result), space_columns)
  expect_equal(out$result$title, "Space 1DXxyRYNejbKM")

  httr2::local_mocked_responses(list(json_response(200, list(meta = list(result_count = 0L)))))
  expect_identical(
    suppressMessages(search_spaces("nothing", bearer_token = "tok")),
    space_schema()
  )
})

test_that("search_spaces validates before any request", {
  no_request()
  expect_error(search_spaces("", bearer_token = "tok"), "`query` must be one search string")
  expect_error(search_spaces("x", state = "ended", bearer_token = "tok"), "\"live\", \"scheduled\" or \"all\"")
  expect_error(search_spaces("x", max_results = 101, bearer_token = "tok"), "between 1 and 100")
  expect_error(search_spaces("x", max_results = 0, bearer_token = "tok"), "between 1 and 100")
  expect_error(search_spaces("x", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("get_space_posts returns one page of posts with the default fields", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    posts_page(1:3)
  })
  out <- discovery_messages(
    get_space_posts("1DXxyRYNejbKM", max_results = 50, bearer_token = "tok")
  )
  expect_match(out$msgs[1], "Reading up to 50 posts, about \\$0.25. Set max_results")
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/spaces/1DXxyRYNejbKM/tweets?", fixed = TRUE)
  expect_match(urls[1], "max_results=50", fixed = TRUE)
  expect_match(urls[1], "tweet.fields=created_at%2Ctext", fixed = TRUE)
  expect_match(urls[1], "expansions=author_id", fixed = TRUE)
  expect_match(urls[1], "user.fields=", fixed = TRUE)
  expect_match(urls[1], "media.fields=", fixed = TRUE)
  expect_match(urls[1], "poll.fields=", fixed = TRUE)
  expect_match(urls[1], "place.fields=", fixed = TRUE)
  expect_type(out$result, "list")
  expect_equal(length(out$result), 1)
  expect_equal(length(out$result[[1]]$data), 3)
  expect_equal(out$result[[1]]$data[[1]]$id, "1")

  no_request()
  expect_error(get_space_posts("1DXxyRYNejbKM", max_results = 101, bearer_token = "tok"),
               "between 1 and 100")
  expect_error(get_space_posts(c("a", "b"), bearer_token = "tok"), "at most 1 id")
  expect_error(get_space_posts("not a space id", bearer_token = "tok"), "letters and digits")
})

# Communities ----------------------------------------------------------------

test_that("get_community hits /2/communities/{id} and returns one row", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(data = api_community("1493446837214187523")))
  })
  out <- discovery_messages(get_community("1493446837214187523", bearer_token = "tok"))
  expect_match(out$msgs[1], "Reading up to 1 communities, about \\$0.01")
  expect_match(urls[1], "/2/communities/1493446837214187523?", fixed = TRUE)
  expect_match(urls[1], "community.fields=access%2Ccreated_at%2Cdescription", fixed = TRUE)

  community <- out$result
  expect_equal(names(community), community_columns)
  expect_equal(nrow(community), 1)
  expect_equal(community$community_id, "1493446837214187523")
  expect_equal(community$name, "Community 1493446837214187523")
  expect_equal(community$access, "Public")
  expect_equal(community$join_policy, "Open")
  expect_equal(community$member_count, 1500L)
  expect_s3_class(community$created_at, "POSIXct")
  expect_equal(attr(community$created_at, "tzone"), "UTC")
})

test_that("get_community stops on an unknown id and validates before any request", {
  httr2::local_mocked_responses(list(json_response(200, list(errors = list(list(
    title = "Not Found Error", detail = "Could not find community with id: [999]."
  ))))))
  expect_error(
    suppressMessages(get_community("999", bearer_token = "tok")),
    "No community found for community_id \"999\". Could not find community"
  )

  no_request()
  expect_error(get_community(1493, bearer_token = "tok"), "string of digits")
  expect_error(get_community("abc", bearer_token = "tok"), "string of digits")
  expect_error(get_community("1493", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("search_communities signs in, hits /2/communities/search and returns the table", {
  local_mocked_bindings(
    authenticate_user = function(...) list(access_token = "tok"),
    .package = "xapir"
  )
  urls <- character(0)
  auth <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    auth <<- c(auth, auth_header(req))
    json_response(200, list(
      data = list(api_community(1), api_community(2)),
      meta = list(next_token = "n2")
    ))
  })
  out <- discovery_messages(search_communities("marketing", max_results = 25))
  expect_match(out$msgs[1], "Reading up to 25 communities, about \\$0.12. Set max_results")
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/communities/search?", fixed = TRUE)
  expect_match(urls[1], "query=marketing", fixed = TRUE)
  expect_match(urls[1], "max_results=25", fixed = TRUE)
  expect_equal(auth, "Bearer tok")
  expect_equal(names(out$result), community_columns)
  expect_equal(out$result$community_id, c("1", "2"))
  expect_equal(out$result$member_count, c(1500L, 1500L))

  httr2::local_mocked_responses(list(json_response(200, list(meta = list(result_count = 0L)))))
  expect_identical(suppressMessages(search_communities("nothing")), community_schema())
})

test_that("search_communities validates before signing in or requesting", {
  local_mocked_bindings(
    authenticate_user = function(...) stop("signed in too early"),
    .package = "xapir"
  )
  no_request()
  expect_error(search_communities("x", max_results = 101), "between 10 and 100")
  expect_error(search_communities("x", max_results = 5), "between 10 and 100")
  expect_error(search_communities(""), "`query` must be one search string")
})

# News -----------------------------------------------------------------------

test_that("search_news hits /2/news/search, prints no cost line and flattens the tags", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    json_response(200, list(data = list(api_news("1989418137272422538")), meta = list(result_count = 1L)))
  })
  out <- discovery_messages(
    search_news("Nebius", max_results = 10, max_age_hours = 48, bearer_token = "tok")
  )
  expect_equal(out$msgs, character(0))
  expect_equal(length(urls), 1)
  expect_match(urls[1], "/2/news/search?", fixed = TRUE)
  expect_match(urls[1], "query=Nebius", fixed = TRUE)
  expect_match(urls[1], "max_results=10", fixed = TRUE)
  expect_match(urls[1], "max_age_hours=48", fixed = TRUE)
  expect_match(urls[1], "news.fields=category%2Ccontexts%2Cdisclaimer", fixed = TRUE)
  expect_false(grepl("cluster_posts_results", urls[1], fixed = TRUE))

  news <- out$result
  expect_equal(names(news), news_columns)
  expect_equal(news$news_id, "1989418137272422538")
  expect_equal(news$name, "Nebius Group Stock Plunges 30%")
  expect_equal(news$hook, "Shares cratered.")
  expect_equal(news$category, "News")
  expect_type(news$keywords, "list")
  expect_equal(news$keywords[[1]], c("Nebius", "stocks"))
  expect_equal(news$contexts[[1]], c("Goldman Sachs", "Nebius Group N.V.", "Stocks", "NBIS"))
  expect_s3_class(news$updated_at, "POSIXct")
  expect_equal(attr(news$updated_at, "tzone"), "UTC")
  expect_equal(format(news$updated_at, "%Y-%m-%d %H:%M"), "2025-11-17 16:21")

  httr2::local_mocked_responses(list(json_response(200, list(data = list(), meta = list(result_count = 0L)))))
  expect_silent(empty <- search_news("nothing", bearer_token = "tok"))
  expect_identical(empty, news_schema())
})

test_that("search_news validates before any request", {
  no_request()
  expect_error(search_news("x", max_results = 101, bearer_token = "tok"), "between 1 and 100")
  expect_error(search_news("x", max_results = 0, bearer_token = "tok"), "between 1 and 100")
  expect_error(search_news("x", max_age_hours = 721, bearer_token = "tok"), "between 1 and 720")
  expect_error(search_news("x", max_age_hours = 0, bearer_token = "tok"), "between 1 and 720")
  expect_error(search_news("  ", bearer_token = "tok"), "`query` must be one search string")
  expect_error(search_news("x", bearer_token = ""), "X_BEARER_TOKEN")
})

test_that("get_news hits /2/news/{id}, returns one row and stops when unknown", {
  urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    if (grepl("/news/1989", req$url, fixed = TRUE)) {
      story <- api_news("1989418137272422538")
      story$last_updated_at_ms <- NULL
      story$updated_at <- "2025-11-17T16:21:41.000Z"
      json_response(200, list(data = story))
    } else {
      json_response(200, list(errors = list(list(
        title = "Not Found Error", detail = "Could not find news with id: [999]."
      ))))
    }
  })
  expect_silent(story <- get_news("1989418137272422538", bearer_token = "tok"))
  expect_match(urls[1], "/2/news/1989418137272422538?", fixed = TRUE)
  expect_match(urls[1], "news.fields=", fixed = TRUE)
  expect_equal(names(story), news_columns)
  expect_equal(nrow(story), 1)
  expect_equal(story$news_id, "1989418137272422538")
  expect_equal(story$summary, "A long summary.")
  expect_false(is.na(story$updated_at))
  expect_error(get_news("999", bearer_token = "tok"), "No news story found for news_id \"999\"")

  no_request()
  expect_error(get_news(1989, bearer_token = "tok"), "string of digits")
  expect_error(get_news("1989", bearer_token = ""), "X_BEARER_TOKEN")
})
