#' Get Post Analytics
#'
#' @description
#' Retrieves engagement metrics over time for up to 100 of the signed-in
#' account's own posts via the [get post analytics
#' endpoint](https://docs.x.com/x-api/posts/get-post-analytics). Needs a
#' user token, so the first call opens a browser window to sign in.
#'
#' The pricing page does not list this endpoint, so the function prints no
#' cost line. Check your usage on the developer console after the first
#' call.
#'
#' @importFrom httr2 req_url_path_append req_url_query
#' @importFrom purrr keep map_dfr
#' @param post_ids A character vector of up to 100 post ids. They must be
#'   posts by the account that signed in.
#' @param start_time The start of the period, as an ISO 8601 string such as
#'   `"2026-09-01T00:00:00Z"` or a date-time object.
#' @param end_time The end of the period, in the same form.
#' @param granularity "total" (the default) for one row per post, or
#'   "hourly", "daily" or "weekly" for one row per post per period.
#' @return A tibble with one row per post per period: `post_id`, `timestamp`
#'   (POSIXct, UTC; `NA` for a total) and one integer column per metric:
#'   `impressions`, `engagements`, `likes`, `reposts`, `replies`, `quotes`,
#'   `bookmarks`, `follows`, `unfollows`, `url_clicks`,
#'   `user_profile_clicks`, `media_views`, `detail_expands`,
#'   `permalink_clicks`, `hashtag_clicks`, `shares`, `app_opens`,
#'   `app_install_attempts`, `email_post` and `unlikes`. The API calls
#'   reposts `retweets`, quotes `quote_tweets` and email shares
#'   `email_tweet`; the table uses the post names. When the API returns
#'   nothing, the same columns with no rows.
#' @examples
#' \dontrun{
#' daily <- get_post_analytics(
#'   post_ids    = c("1234567890123456789", "1234567890123456790"),
#'   start_time  = "2026-09-01T00:00:00Z",
#'   end_time    = "2026-09-08T00:00:00Z",
#'   granularity = "daily"
#' )
#' }
#' @export
get_post_analytics <- function(
  post_ids,
  start_time,
  end_time,
  granularity = "total"
) {

  check_post_ids(post_ids, max_ids = 100)
  start_time <- check_analytics_time(start_time, "start_time")
  end_time   <- check_analytics_time(end_time, "end_time")
  choices <- c("hourly", "daily", "weekly", "total")
  ok <- is.character(granularity) && length(granularity) == 1 &&
    !is.na(granularity) && granularity %in% choices
  if (!ok) {
    stop(
      "`granularity` must be \"hourly\", \"daily\", \"weekly\" or \"total\".",
      call. = FALSE
    )
  }

  token <- authenticate_user()

  req <- x_request(token$access_token) |>
    req_url_path_append("tweets", "analytics") |>
    req_url_query(
      ids              = str_c(post_ids, collapse = ","),
      start_time       = start_time,
      end_time         = end_time,
      granularity      = granularity,
      analytics.fields = str_c(
        c("id", "timestamp", "timestamped_metrics", names(analytics_metrics())),
        collapse = ","
      )
    )

  # A 403 here names a missing Project, but the app is usually attached to
  # one: the endpoint is closed to some accounts, and the API's own text
  # sends people looking for the wrong thing.
  page <- tryCatch(
    x_perform(req),
    httr2_http_403 = function(e) {
      stop(
        conditionMessage(e), "\n",
        "Post analytics returned 403. That usually means the endpoint is not ",
        "open to this account or app, whatever the message says about a ",
        "Project.",
        call. = FALSE
      )
    }
  )

  warn_partial_errors(page$errors, what = "posts")
  analytics_table(page$data)
}

# A time is sent as the API wants it: an ISO 8601 string is passed through
# and a Date or date-time goes through iso_8601(), so a bare Date means
# local midnight here as it does everywhere else in the package.
check_analytics_time <- function(x, arg) {
  if (inherits(x, c("Date", "POSIXt"))) {
    return(iso_8601(x))
  }
  ok <- is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  if (!ok) {
    stop(
      "`", arg, "` must be one ISO 8601 string, such as ",
      "\"2026-09-01T00:00:00Z\", or a date-time.",
      call. = FALSE
    )
  }
  x
}

# The metrics the API reports, named as it names them, with the column each
# one fills. Only three differ: the API still says "tweet".
analytics_metrics <- function() {
  c(
    impressions          = "impressions",
    engagements          = "engagements",
    likes                = "likes",
    retweets             = "reposts",
    replies              = "replies",
    quote_tweets         = "quotes",
    bookmarks            = "bookmarks",
    follows              = "follows",
    unfollows            = "unfollows",
    url_clicks           = "url_clicks",
    user_profile_clicks  = "user_profile_clicks",
    media_views          = "media_views",
    detail_expands       = "detail_expands",
    permalink_clicks     = "permalink_clicks",
    hashtag_clicks       = "hashtag_clicks",
    shares               = "shares",
    app_opens            = "app_opens",
    app_install_attempts = "app_install_attempts",
    email_tweet          = "email_post",
    unlikes              = "unlikes"
  )
}

analytics_schema <- function() {
  metrics <- analytics_metrics()
  columns <- c(
    list(
      post_id   = character(0),
      timestamp = as.POSIXct(character(0), tz = "UTC")
    ),
    setNames(rep(list(integer(0)), length(metrics)), unname(metrics))
  )
  as_tibble(columns)
}

# One row: the post id, one timestamp and the metrics from one bucket.
analytics_row <- function(post_id, timestamp, metrics) {
  names_api <- names(analytics_metrics())
  values <- lapply(names_api, function(m) {
    as.integer(metrics[[m]] %||% NA_integer_)
  })
  columns <- c(
    list(
      post_id   = as.character(post_id %||% NA_character_),
      timestamp = parse_x_time(timestamp)
    ),
    setNames(values, unname(analytics_metrics()))
  )
  as_tibble(columns)
}

# One post's entry becomes one row per timestamped bucket. An entry with no
# buckets gives one row from the metrics at its top level.
analytics_rows <- function(x) {
  buckets <- keep(x$timestamped_metrics %||% list(), is.list)
  if (length(buckets) == 0) {
    return(analytics_row(x$id, x$timestamp, x))
  }
  map_dfr(buckets, function(b) {
    analytics_row(x$id, b$timestamp, b$metrics %||% list())
  })
}

analytics_table <- function(data) {
  data <- keep(data %||% list(), ~ is.list(.x) && !is.null(.x$id))
  if (length(data) == 0) {
    return(analytics_schema())
  }
  map_dfr(data, analytics_rows)
}
