tl <- tesla_pages()

test_that("extract_user has is_identity_verified and url, and every column fills", {
  user <- extract_user(tl)
  expect_equal(names(user), names(user_schema()))
  expect_equal(ncol(user), 24)
  expect_equal(nrow(user), length(unique(user$user_id)))
  expect_type(user$is_identity_verified, "logical")
  expect_false(any(is.na(user$is_identity_verified)))
  expect_type(user$url, "character")
  expect_gt(sum(!is.na(user$url)), 0)
  expect_equal(attr(user$created_at, "tzone"), "UTC")
  always_filled <- c("created_at", "username", "name", "description",
                     "followers_count", "following_count", "post_count",
                     "listed_count", "like_count", "protected", "verified",
                     "verified_type", "profile_image_url", "user_id")
  for (col in always_filled) {
    expect_false(any(is.na(user[[col]])), info = col)
  }
  # the newer columns are there with their types even where the fixture,
  # pulled before they joined the defaults, leaves them NA
  expect_type(user$media_count, "integer")
  expect_type(user$verified_followers_count, "integer")
  expect_type(user$subscription_type, "character")
  expect_type(user$parody, "logical")
  expect_type(user$profile_banner_url, "character")
  expect_type(user$pinned_post_id, "character")
})

test_that("extract_post_edited_post_id has no self-referencing rows", {
  edits <- extract_post_edited_post_id(tl)
  expect_gt(nrow(edits), 0)
  expect_false(any(edits$post_id == edits$edited_post_id))
  expect_type(edits$post_id, "character")
  expect_type(edits$edited_post_id, "character")
})

test_that("extract_post_poll_option keeps the post_id of a poll on a quoted post", {
  polls <- extract_post_poll_option(list(poll_page()))
  expect_equal(nrow(polls), 2)
  expect_false(any(is.na(polls$post_id)))
  expect_equal(unique(polls$post_id), "2")
  expect_equal(polls$label, c("Yes", "No"))
})

test_that("extract_post_poll_option returns no rows on a timeline without polls", {
  polls <- extract_post_poll_option(tl)
  expect_equal(nrow(polls), 0)
  expect_true("post_id" %in% names(polls))
})

test_that("extract_post_media has alt_text", {
  media <- extract_post_media(tl)
  expect_gt(nrow(media), 0)
  expect_true("alt_text" %in% names(media))
  expect_type(media$alt_text, "character")
  expect_false(any(is.na(media$post_id)))
})

# ---- child tables: shared fixtures and helpers ------------------------------

# One hand-built page with every kind of child data. Post "1" is in data and
# carries a url, a mention, a context annotation, an entity annotation and a
# photo. Post "2" lives only in includes$tweets (post 1 quotes it) and carries
# a video with three mp4 variants, a url, a hashtag, a cashtag, a mention, a
# context annotation, an entity annotation, a place, an edit history and a
# poll. Every child table therefore has at least one row that exists only
# because includes$tweets was read.
entity_page <- function() {
  list(
    data = list(
      list(
        id = "1", author_id = "u1", text = "quoting @two https://t.co/a",
        created_at = "2026-09-01T10:00:00.000Z",
        referenced_tweets = list(list(type = "quoted", id = "2")),
        attachments = list(media_keys = list("3_photo")),
        entities = list(
          urls = list(list(start = 12L, end = 25L, url = "https://t.co/a",
                           expanded_url = "https://example.com/a",
                           display_url = "example.com/a", status = 200L,
                           title = "A", description = "page a",
                           images = list(list(url = "https://img/a", width = 10L, height = 10L)))),
          mentions = list(list(start = 8L, end = 12L, username = "two", id = "u2")),
          annotations = list(list(start = 0L, end = 6L, probability = 0.5,
                                  type = "Product", normalized_text = "quoting"))
        ),
        context_annotations = list(
          list(domain = list(id = "46", name = "Business Taxonomy", description = "d46"),
               entity = list(id = "e1", name = "Cars", description = "cars"))
        ),
        edit_history_tweet_ids = list("1")
      )
    ),
    includes = list(
      tweets = list(
        list(
          id = "2", author_id = "u2", text = "#FSD $TSLA @one https://t.co/b",
          created_at = "2026-09-01T09:00:00.000Z",
          attachments = list(media_keys = list("7_video"), poll_ids = list("p1")),
          entities = list(
            urls = list(list(start = 16L, end = 29L, url = "https://t.co/b",
                             expanded_url = "https://example.com/b",
                             display_url = "example.com/b")),
            hashtags = list(list(start = 0L, end = 4L, tag = "FSD")),
            cashtags = list(list(start = 5L, end = 10L, tag = "TSLA")),
            mentions = list(list(start = 11L, end = 15L, username = "one", id = "u1")),
            annotations = list(list(start = 6L, end = 9L, probability = 0.9,
                                    type = "Organization", normalized_text = "TSLA"))
          ),
          context_annotations = list(
            list(domain = list(id = "47", name = "Brand", description = "d47"),
                 entity = list(id = "e2", name = "Tesla", description = "tesla"))
          ),
          geo = list(place_id = "pl1"),
          edit_history_tweet_ids = list("2", "2-old")
        )
      ),
      media = list(
        list(media_key = "3_photo", type = "photo", url = "https://img/photo",
             width = 100L, height = 50L, alt_text = "a photo"),
        list(media_key = "7_video", type = "video", duration_ms = 1000L,
             width = 1280L, height = 720L, preview_image_url = "https://img/preview",
             public_metrics = list(view_count = 42L),
             variants = list(
               list(content_type = "application/x-mpegURL", url = "https://v/pl.m3u8"),
               list(content_type = "video/mp4", bit_rate = 256000L, url = "https://v/low.mp4"),
               list(content_type = "video/mp4", bit_rate = 2176000L, url = "https://v/high.mp4"),
               list(content_type = "video/mp4", bit_rate = 832000L, url = "https://v/mid.mp4")
             ))
      ),
      places = list(
        list(id = "pl1", full_name = "Austin, TX", country = "United States",
             country_code = "US", place_type = "city",
             geo = list(bbox = list(-97.9, 30.1, -97.5, 30.5)))
      ),
      polls = list(
        list(id = "p1", duration_minutes = 60L, end_datetime = "2026-09-01T10:00:00.000Z",
             voting_status = "closed",
             options = list(list(position = 1L, label = "Yes", votes = 3L),
                            list(position = 2L, label = "No", votes = 1L)))
      ),
      users = list(list(id = "u1", username = "one"), list(id = "u2", username = "two"))
    ),
    meta = list(result_count = 1L)
  )
}

empty_timeline <- list(list(meta = list(result_count = 0L)))

col_type <- function(x) if (inherits(x, "POSIXct")) "POSIXct" else typeof(x)

# The documented schema of every child table: column name -> type.
child_schemas <- list(
  extract_post_media = c(
    post_id = "character", media_id = "character", type = "character",
    view_count = "integer", duration_ms = "integer", height = "integer",
    width = "integer", preview_image_url = "character", url = "character",
    alt_text = "character", bit_rate = "integer"
  ),
  extract_post_url = c(
    post_id = "character", start = "integer", end = "integer", url = "character",
    expanded_url = "character", unwound_url = "character", display_url = "character",
    title = "character", description = "character", status = "integer",
    image_url = "character"
  ),
  extract_post_hashtag = c(
    post_id = "character", hashtag = "character", start = "integer", end = "integer"
  ),
  extract_post_mention = c(
    post_id = "character", username = "character", user_id = "character",
    start = "integer", end = "integer"
  ),
  extract_post_cashtag = c(
    post_id = "character", tag = "character", start = "integer", end = "integer"
  ),
  extract_post_context = c(
    post_id = "character", domain_id = "character", domain_name = "character",
    domain_description = "character", entity_id = "character",
    entity_name = "character", entity_description = "character"
  ),
  extract_post_entity_annotation = c(
    post_id = "character", normalized_text = "character", type = "character",
    probability = "double", start = "integer", end = "integer"
  ),
  extract_post_place = c(
    post_id = "character", place_id = "character", full_name = "character",
    country = "character", country_code = "character", place_type = "character",
    west_longitude = "double", south_latitude = "double",
    east_longitude = "double", north_latitude = "double"
  ),
  extract_post_edited_post_id = c(
    post_id = "character", edited_post_id = "character"
  ),
  extract_post_poll_option = c(
    post_id = "character", poll_id = "character", position = "integer",
    label = "character", votes = "integer", duration_minutes = "integer",
    end_datetime = "POSIXct", voting_status = "character"
  )
)

# How many rows each child table should have for one raw post: the number of
# distinct (post_id, key) pairs, counted straight from the JSON so the test
# does not depend on the extractor. The API repeats some entries (a context
# annotation twice, or one url entity per attached photo), and the tables
# keep each once.
n_keys <- function(items, key) length(unique(vapply(items, key, "")))

expected_rows <- list(
  extract_post_media             = function(p) n_keys(p$attachments$media_keys, identity),
  extract_post_url               = function(p) n_keys(p$entities$urls, function(u) paste(u$start, u$end, u$url)),
  extract_post_hashtag           = function(p) n_keys(p$entities$hashtags, function(h) paste(h$start, h$end, h$tag)),
  extract_post_mention           = function(p) n_keys(p$entities$mentions, function(m) paste(m$start, m$end, m$id)),
  extract_post_cashtag           = function(p) n_keys(p$entities$cashtags, function(ct) paste(ct$start, ct$end, ct$tag)),
  extract_post_context           = function(p) n_keys(p$context_annotations, function(a) paste(a$domain$id, a$entity$id)),
  extract_post_entity_annotation = function(p) n_keys(p$entities$annotations, function(a) paste(a$start, a$end, a$type)),
  extract_post_place             = function(p) length(p$geo$place_id),
  extract_post_edited_post_id    = function(p) length(setdiff(unlist(p$edit_history_tweet_ids), p$id)),
  extract_post_poll_option       = function(p) if (length(p$attachments$poll_ids)) 2L else 0L
)

# The raw posts of a timeline with each id kept once, data first.
raw_unique_posts <- function(timeline) {
  posts <- c(
    unlist(lapply(timeline, function(p) p$data), recursive = FALSE),
    unlist(lapply(timeline, function(p) p$includes$tweets), recursive = FALSE)
  )
  posts[!duplicated(vapply(posts, `[[`, "", "id"))]
}

# ---- (a) zero-row results keep the documented schema -----------------------

for (fn_name in names(child_schemas)) {
  test_that(paste(fn_name, "gives a typed zero-row tibble on an empty timeline"), {
    fn    <- get(fn_name)
    empty <- fn(empty_timeline)
    expect_s3_class(empty, "tbl_df")
    expect_equal(nrow(empty), 0)
    expect_equal(names(empty), names(child_schemas[[fn_name]]))
    expect_equal(vapply(empty, col_type, ""), child_schemas[[fn_name]])
    # a non-empty result has the same columns and types
    full <- fn(list(entity_page()))
    expect_equal(names(full), names(child_schemas[[fn_name]]))
    expect_equal(vapply(full, col_type, ""), child_schemas[[fn_name]])
  })
}

test_that("end_datetime is UTC on both empty and filled poll tables", {
  expect_equal(attr(extract_post_poll_option(empty_timeline)$end_datetime, "tzone"), "UTC")
  expect_equal(attr(extract_post_poll_option(list(entity_page()))$end_datetime, "tzone"), "UTC")
})

# ---- (b) posts that live only in includes$tweets ----------------------------

for (fn_name in names(child_schemas)) {
  test_that(paste(fn_name, "reads includes$tweets by default and not when asked"), {
    fn   <- get(fn_name)
    page <- list(entity_page())
    with_included <- fn(page)
    expect_true("2" %in% with_included$post_id)
    own_only <- fn(page, include_referenced_posts = FALSE)
    expect_false("2" %in% own_only$post_id)
    expect_equal(names(own_only), names(child_schemas[[fn_name]]))
  })
}

test_that("include_referenced_posts = FALSE keeps only data posts on the Tesla pages", {
  data_ids <- vapply(unlist(lapply(tl, function(p) p$data), recursive = FALSE), `[[`, "", "id")
  for (fn_name in names(child_schemas)) {
    own <- get(fn_name)(tl, include_referenced_posts = FALSE)
    expect_true(all(own$post_id %in% data_ids), info = fn_name)
  }
})

# ---- (c) post_id is character and never NA ----------------------------------

for (fn_name in names(child_schemas)) {
  test_that(paste(fn_name, "has a character post_id with no NA"), {
    fn <- get(fn_name)
    for (out in list(fn(tl), fn(list(entity_page())), fn(list(poll_page())))) {
      expect_type(out$post_id, "character")
      expect_false(any(is.na(out$post_id)))
    }
  })
}

# ---- (d) one row per (post, item), no duplicates across pages ---------------

for (fn_name in names(child_schemas)) {
  test_that(paste(fn_name, "has one row per item across the Tesla pages"), {
    out      <- get(fn_name)(tl)
    expected <- sum(vapply(raw_unique_posts(tl), expected_rows[[fn_name]], 1L))
    expect_equal(nrow(out), expected)
    expect_equal(nrow(out), nrow(dplyr::distinct(out)))
  })
}

test_that("a post on two pages gives the same rows as on one page", {
  once  <- list(entity_page())
  twice <- list(entity_page(), entity_page())
  for (fn_name in names(child_schemas)) {
    fn <- get(fn_name)
    expect_equal(fn(twice), fn(once), info = fn_name)
    expect_gt(nrow(fn(once)), 0, label = fn_name)
  }
})

# ---- (e) media picks the highest bit-rate mp4 -------------------------------

test_that("extract_post_media picks the highest bit-rate mp4 variant", {
  media <- extract_post_media(list(entity_page()))
  video <- media[media$media_id == "7_video", ]
  expect_equal(nrow(video), 1)
  expect_equal(video$post_id, "2")
  expect_equal(video$url, "https://v/high.mp4")
  expect_equal(video$bit_rate, 2176000L)
  expect_equal(video$view_count, 42L)
  expect_equal(video$preview_image_url, "https://img/preview")
  photo <- media[media$media_id == "3_photo", ]
  expect_equal(photo$post_id, "1")
  expect_equal(photo$url, "https://img/photo")
  expect_true(is.na(photo$bit_rate))
  expect_equal(photo$alt_text, "a photo")
  expect_equal(photo$height, 50L)
  expect_equal(photo$width, 100L)
})

test_that("extract_post_media bit_rate is the max mp4 bit rate on the Tesla pages", {
  raw_media <- unlist(lapply(tl, function(p) p$includes$media), recursive = FALSE)
  max_rate  <- vapply(raw_media, function(m) {
    mp4 <- Filter(function(v) identical(v$content_type, "video/mp4"), m$variants %||% list())
    if (length(mp4) == 0) NA_integer_ else max(vapply(mp4, `[[`, 1L, "bit_rate"))
  }, 1L)
  names(max_rate) <- vapply(raw_media, `[[`, "", "media_key")
  media <- extract_post_media(tl)
  expect_gt(sum(!is.na(media$bit_rate)), 0)
  expect_equal(media$bit_rate, unname(max_rate[media$media_id]))
})

test_that("extract_post_media keeps height and width when a media item lacks them", {
  page <- entity_page()
  page$includes$media[[1]]$height <- NULL
  page$includes$media[[1]]$width  <- NULL
  media <- extract_post_media(list(page))
  expect_true(all(c("height", "width") %in% names(media)))
  expect_type(media$height, "integer")
  expect_true(is.na(media$height[media$media_id == "3_photo"]))
})

# ---- the rest of the hand-built page comes through --------------------------

test_that("extract_post_place joins the place details and bounding box", {
  place <- extract_post_place(list(entity_page()))
  expect_equal(nrow(place), 1)
  expect_equal(place$post_id, "2")
  expect_equal(place$full_name, "Austin, TX")
  expect_equal(place$west_longitude, -97.9)
  expect_equal(place$north_latitude, 30.5)
})

test_that("extract_post_place keeps a tagged post whose place is not in includes", {
  page <- entity_page()
  page$includes$places <- NULL
  place <- extract_post_place(list(page))
  expect_equal(place$post_id, "2")
  expect_equal(place$place_id, "pl1")
  expect_true(is.na(place$full_name))
})

test_that("extract_post_url reads the first preview image and NA when none", {
  url <- extract_post_url(list(entity_page()))
  expect_equal(url$image_url[url$post_id == "1"], "https://img/a")
  expect_true(is.na(url$image_url[url$post_id == "2"]))
  expect_true(is.na(url$unwound_url[url$post_id == "2"]))
})

test_that("hashtag, cashtag, mention and annotation rows carry their positions", {
  page <- list(entity_page())
  expect_equal(extract_post_hashtag(page)$hashtag, "FSD")
  expect_equal(extract_post_hashtag(page)$start, 0L)
  expect_equal(extract_post_cashtag(page)$tag, "TSLA")
  mention <- extract_post_mention(page)
  expect_setequal(mention$username, c("two", "one"))
  expect_equal(mention$user_id[mention$post_id == "1"], "u2")
  annotation <- extract_post_entity_annotation(page)
  expect_equal(annotation$probability[annotation$post_id == "2"], 0.9)
  context <- extract_post_context(page)
  expect_equal(context$entity_name[context$post_id == "2"], "Tesla")
})

test_that("extract_post_edited_post_id drops self rows and keeps earlier versions", {
  edits <- extract_post_edited_post_id(list(entity_page()))
  expect_equal(nrow(edits), 1)
  expect_equal(edits$post_id, "2")
  expect_equal(edits$edited_post_id, "2-old")
})
