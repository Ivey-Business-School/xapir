tl <- tesla_pages()

test_that("extract_user has is_identity_verified and url, and every column fills", {
  user <- extract_user(tl)
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
