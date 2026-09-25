## Internal helpers shared by every function that returns a table of users,
## lists or trends. Nothing in this file is exported.
##
## Each table has one builder that turns a single parsed object into a
## one-row tibble, and one zero-row schema with the same column types. The
## schema is what a reader returns when the API sends nothing back, so a
## column is always there to select on, whether or not anything was found.

#' @importFrom purrr keep map_chr map_dfr pluck
#' @importFrom lubridate ymd_hms
NULL

# Cost -----------------------------------------------------------------------

# The price per user comes from x_price("users") in utils-request.R.
# One line, before the first request, so the reader knows what the call can
# cost. Every user returned is billed, so the cap is the worst case.
# `cap_arg` names the argument to change when the count is a cap rather than
# the exact number of users asked for.
announce_user_cap <- function(n, cap_arg = NULL, what = "users", owned = FALSE) {
  price <- x_price(if (owned) "owned" else what)
  text <- sprintf(
    "Reading up to %s users, about $%s%s.",
    format(n, big.mark = ",", scientific = FALSE),
    dollars(n * price),
    if (owned) " (your own data)" else ""
  )
  if (!is.null(cap_arg)) {
    text <- paste0(text, " Set ", cap_arg, " to change this.")
  }
  message(text)
}

# Guardrails -----------------------------------------------------------------

# A batch of up to 100 usernames. A leading "@" is dropped, because that is
# how handles are written everywhere but in the API.
check_usernames <- function(usernames) {
  ok <- is.character(usernames) && length(usernames) >= 1 &&
    !anyNA(usernames) && all(nzchar(usernames))
  if (!ok) {
    stop(
      "`usernames` must be a character vector of one or more handles, ",
      "such as c(\"Tesla\", \"XDevelopers\").",
      call. = FALSE
    )
  }
  if (length(usernames) > 100) {
    stop(
      "`usernames` can hold at most 100 handles per call. ",
      "Split the vector and call the function once per 100.",
      call. = FALSE
    )
  }
  sub("^@", "", usernames)
}

# A batch of up to 100 user ids, each a string of digits.
check_user_ids <- function(user_ids) {
  ok <- is.character(user_ids) && length(user_ids) >= 1 &&
    !anyNA(user_ids) && all(grepl("^[0-9]+$", user_ids))
  if (!ok) {
    stop(
      "`user_ids` must be a character vector of one or more ids, each a ",
      "string of digits, such as c(\"783214\", \"2244994945\"). ",
      "Keep ids as text: as numbers they lose digits.",
      call. = FALSE
    )
  }
  if (length(user_ids) > 100) {
    stop(
      "`user_ids` can hold at most 100 ids per call. ",
      "Split the vector and call the function once per 100.",
      call. = FALSE
    )
  }
  user_ids
}

# A list id, one string of digits.
check_list_id <- function(list_id) {
  ok <- is.character(list_id) && length(list_id) == 1 &&
    !is.na(list_id) && grepl("^[0-9]+$", list_id)
  if (!ok) {
    stop(
      "`list_id` must be one string of digits, such as ",
      "\"1146654567674912769\". Keep ids as text: as numbers they lose digits.",
      call. = FALSE
    )
  }
  invisible(list_id)
}

# The API answers a batch lookup with a 200 even when some of the items
# could not be found. Those go in `errors`, next to the `data` that worked.
# One warning names each of them so the reader knows which rows are missing.
warn_partial_errors <- function(errors, what = "users") {
  if (length(errors) == 0) {
    return(invisible(NULL))
  }
  # A "Field Authorization Error" means one field was refused, not one
  # item: the item came back with that field missing. Those get their own
  # warning naming the fields, and are not counted as unread items.
  detail   <- map_chr(errors, ~ .x$detail %||% .x$title %||% "Unknown error.")
  is_field <- map_lgl(errors, ~ identical(.x$title, "Field Authorization Error")) |
    grepl("not authorized to access '", detail, fixed = TRUE)

  if (any(is_field)) {
    fields <- unique(map_chr(errors[is_field], function(e) {
      e$value %||% sub(".*access '([^']+)'.*", "\\1", e$detail %||% "")
    }))
    fields <- fields[nzchar(fields)]
    warning(
      "This token cannot read ", length(fields), " of the fields asked for, ",
      "so they are NA: ", paste(fields, collapse = ", "), ". ",
      "Drop them from `user_fields` to silence this.",
      call. = FALSE
    )
  }

  if (any(!is_field)) {
    warning(
      sum(!is_field), " of the ", what, " could not be read. ",
      paste(detail[!is_field], collapse = " "),
      call. = FALSE
    )
  }
  invisible(NULL)
}

# Dates from the API are ISO 8601 in UTC. NA stays NA, and a string that
# does not parse becomes NA without a warning.
parse_x_time <- function(x) {
  ymd_hms(x %||% NA_character_, tz = "UTC", quiet = TRUE)
}

# Users ----------------------------------------------------------------------

# The 24 columns every user table has, in this order, with no rows.
user_schema <- function() {
  tibble(
    created_at               = as.POSIXct(character(0), tz = "UTC"),
    username                 = character(0),
    name                     = character(0),
    description              = character(0),
    followers_count          = integer(0),
    following_count          = integer(0),
    post_count               = integer(0),
    listed_count             = integer(0),
    like_count               = integer(0),
    media_count              = integer(0),
    protected                = logical(0),
    verified                 = logical(0),
    verified_type            = character(0),
    verified_followers_count = integer(0),
    subscription_type        = character(0),
    parody                   = logical(0),
    is_identity_verified     = logical(0),
    location                 = character(0),
    profile_image_url        = character(0),
    profile_banner_url       = character(0),
    link_in_bio              = character(0),
    url                      = character(0),
    pinned_post_id           = character(0),
    user_id                  = character(0)
  )
}

# One row of the user table from one parsed user. Every field the API may
# leave out gets a typed NA, so rows always bind.
user_row <- function(x) {
  metrics <- x$public_metrics %||% list()

  # The profile link comes twice: `url` is the t.co address and the display
  # form sits in entities.
  link_in_bio <- pluck(x, "entities", "url", "urls", 1, "display_url")
  url <- x$url %||% NA_character_
  # The API sends "" for an account with no profile link.
  if (identical(url, "")) {
    url <- NA_character_
  }

  tibble(
    created_at               = parse_x_time(x$created_at),
    username                 = as.character(x$username %||% NA_character_),
    name                     = as.character(x$name %||% NA_character_),
    description              = as.character(x$description %||% NA_character_),
    followers_count          = as.integer(metrics$followers_count %||% NA_integer_),
    following_count          = as.integer(metrics$following_count %||% NA_integer_),
    post_count               = as.integer(metrics$tweet_count %||% NA_integer_),
    listed_count             = as.integer(metrics$listed_count %||% NA_integer_),
    like_count               = as.integer(metrics$like_count %||% NA_integer_),
    media_count              = as.integer(metrics$media_count %||% NA_integer_),
    protected                = as.logical(x$protected %||% NA),
    verified                 = as.logical(x$verified %||% NA),
    verified_type            = as.character(x$verified_type %||% NA_character_),
    verified_followers_count = as.integer(x$verified_followers_count %||% NA_integer_),
    subscription_type        = as.character(x$subscription_type %||% NA_character_),
    parody                   = as.logical(x$parody %||% NA),
    is_identity_verified     = as.logical(x$is_identity_verified %||% NA),
    location                 = as.character(x$location %||% NA_character_),
    profile_image_url        = as.character(x$profile_image_url %||% NA_character_),
    profile_banner_url       = as.character(x$profile_banner_url %||% NA_character_),
    link_in_bio              = as.character(link_in_bio %||% NA_character_),
    url                      = as.character(url),
    # The API still calls it pinned_tweet_id.
    pinned_post_id           = as.character(x$pinned_tweet_id %||% NA_character_),
    user_id                  = as.character(x$id %||% NA_character_)
  )
}

# A list of parsed users to one table, one row per user id. An entry with
# no id is not a user and is dropped. Returns the empty schema when nothing
# is left, and never warns.
users_table <- function(users) {
  users <- keep(users %||% list(), ~ is.list(.x) && !is.null(.x$id))
  if (length(users) == 0) {
    return(user_schema())
  }
  map_dfr(users, user_row) |>
    distinct(user_id, .keep_all = TRUE)
}

# Lists ----------------------------------------------------------------------

list_schema <- function() {
  tibble(
    list_id        = character(0),
    list_name      = character(0),
    description    = character(0),
    created_at     = as.POSIXct(character(0), tz = "UTC"),
    follower_count = integer(0),
    member_count   = integer(0),
    private        = logical(0),
    owner_id       = character(0)
  )
}

list_row <- function(x) {
  tibble(
    list_id        = as.character(x$id %||% NA_character_),
    list_name      = as.character(x$name %||% NA_character_),
    description    = as.character(x$description %||% NA_character_),
    created_at     = parse_x_time(x$created_at),
    follower_count = as.integer(x$follower_count %||% NA_integer_),
    member_count   = as.integer(x$member_count %||% NA_integer_),
    private        = as.logical(x$private %||% NA),
    owner_id       = as.character(x$owner_id %||% NA_character_)
  )
}

lists_table <- function(lists) {
  lists <- keep(lists %||% list(), ~ is.list(.x) && !is.null(.x$id))
  if (length(lists) == 0) {
    return(list_schema())
  }
  out <- map_dfr(lists, list_row)
  out[!duplicated(out$list_id), ]
}

# Trends ---------------------------------------------------------------------

trend_schema <- function() {
  tibble(
    trend_name = character(0),
    post_count = integer(0)
  )
}

trend_row <- function(x) {
  tibble(
    trend_name = as.character(x$trend_name %||% NA_character_),
    post_count = as.integer(x$tweet_count %||% NA_integer_)
  )
}

trends_table <- function(trends) {
  trends <- keep(trends %||% list(), is.list)
  if (length(trends) == 0) {
    return(trend_schema())
  }
  map_dfr(trends, trend_row)
}
