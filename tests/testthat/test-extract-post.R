tl   <- tesla_pages()
post <- extract_post(tl)

raw_posts    <- unlist(lapply(tl, function(p) p$data), recursive = FALSE)
raw_included <- unlist(lapply(tl, function(p) p$includes$tweets), recursive = FALSE)
ids_data     <- vapply(raw_posts, `[[`, "", "id")
ids_included <- vapply(raw_included, `[[`, "", "id")

test_that("one row per post id, even when a post is in data and includes", {
  expect_gt(length(intersect(ids_data, ids_included)), 0)
  expect_equal(nrow(post), length(unique(post$post_id)))
  expect_equal(nrow(post), length(union(ids_data, ids_included)))
})

test_that("the data copy wins over the includes copy", {
  shared <- intersect(ids_data, ids_included)[1]
  from_data <- raw_posts[[match(shared, ids_data)]]
  expect_equal(post$text[post$post_id == shared],
               from_data$note_tweet$text %||% from_data$text)
})

test_that("long posts carry the full text and is_long_post is TRUE", {
  long_raw <- Filter(function(x) !is.null(x$note_tweet), raw_posts)
  expect_gt(length(long_raw), 0)
  for (x in long_raw) {
    row <- post[post$post_id == x$id, ]
    expect_true(row$is_long_post)
    expect_equal(row$text, x$note_tweet$text)
    expect_gt(nchar(row$text), nchar(x$text))
  }
  expect_type(post$is_long_post, "logical")
  expect_false(any(is.na(post$is_long_post)))
  expect_false("note_tweet" %in% names(post))
})

test_that("reposts have NA like_count and their own impression_count", {
  reposts <- post[!is.na(post$reposted), ]
  expect_gt(nrow(reposts), 0)
  expect_true(all(is.na(reposts$like_count)))
  expect_true(all(is.na(reposts$reply_count)))
  expect_true(all(is.na(reposts$quote_count)))
  expect_true(all(is.na(reposts$bookmark_count)))
  expect_true(all(is.na(reposts$repost_count)))
  expect_true(all(!is.na(reposts$impression_count)))
  expect_equal(as.character(unique(reposts$post_type)), "Repost")
})

test_that("post_url uses the author's handle", {
  user <- extract_user(tl)
  handle <- user$username[match(post$user_id, user$user_id)]
  expect_false(any(is.na(handle)))
  expect_equal(post$post_url, paste0("https://x.com/", handle, "/status/", post$post_id))
})

test_that("post_url falls back to i/web when the author is not in includes", {
  page <- poll_page()
  page$includes$users <- NULL
  out <- extract_post(list(page))
  expect_equal(out$post_url, paste0("https://x.com/i/web/status/", out$post_id))
})

test_that("created_at is UTC by default and tz converts it", {
  expect_s3_class(post$created_at, "POSIXct")
  expect_equal(attr(post$created_at, "tzone"), "UTC")
  local <- extract_post(tl, tz = "America/Toronto")
  expect_equal(attr(local$created_at, "tzone"), "America/Toronto")
  expect_equal(as.numeric(local$created_at), as.numeric(post$created_at))
})

test_that("lang, possibly_sensitive and article_title are present", {
  expect_type(post$lang, "character")
  expect_type(post$possibly_sensitive, "logical")
  expect_false(any(is.na(post$possibly_sensitive)))
  # the fixture holds at least one X article
  expect_true("article_title" %in% names(post))
  expect_gt(sum(!is.na(post$article_title)), 0)
  # a page with no article has no article_title column
  expect_false("article_title" %in% names(extract_post(list(poll_page()))))
})

test_that("ids are character", {
  for (col in c("post_id", "user_id", "conversation_id", "in_reply_to_user_id",
                "reposted", "quoted", "replied_to")) {
    expect_type(post[[col]], "character")
  }
})

test_that("a post that replies and quotes is one row with both ids", {
  page <- poll_page()
  page$data[[1]]$referenced_tweets <- list(
    list(type = "replied_to", id = "9"),
    list(type = "quoted", id = "2")
  )
  out <- extract_post(list(page))
  expect_equal(sum(out$post_id == "1"), 1)
  expect_equal(out$replied_to[out$post_id == "1"], "9")
  expect_equal(out$quoted[out$post_id == "1"], "2")
})

test_that("include_referenced_posts = FALSE keeps only the data posts", {
  own <- extract_post(tl, include_referenced_posts = FALSE)
  expect_setequal(own$post_id, ids_data)
})

test_that("an empty page gives a zero-row tibble with the post columns", {
  empty <- extract_post(list(list(meta = list(result_count = 0L))))
  expect_equal(nrow(empty), 0)
  expect_true(all(c("created_at", "text", "is_long_post", "post_type",
                    "post_url", "post_id") %in% names(empty)))
})
