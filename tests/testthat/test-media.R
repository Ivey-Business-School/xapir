# upload_media() walks the initialize / append / finalize / status steps.
# Nothing here calls the API: authenticate_user() is stubbed, every request
# is answered by a mock, and the wait between status polls is recorded
# instead of slept.

# A 10,000-byte file with a png extension. The bytes are not a real image;
# the mock never looks at them.
fake_png <- function(env = parent.frame()) {
  tmp <- withr::local_tempfile(fileext = ".png", .local_envir = env)
  writeBin(as.raw(rep(1:250, 40)), tmp)
  tmp
}

# Answers each step of the upload flow. `states` are the processing states
# the STATUS endpoint returns in turn; `finalize_info` is what finalize
# says. Chunk sizes are read at response time, because the chunk's
# temporary file is removed once the request has been answered.
media_responder <- function(states = character(0), finalize_info = NULL,
                            chunk_sizes = NULL) {
  polls <- 0
  function(req) {
    url <- req$url
    if (grepl("/media/upload/initialize$", url)) {
      json_response(200, list(data = list(
        id = "555", media_key = "3_555", expires_after_secs = 86400
      )))
    } else if (grepl("/media/upload/555/append$", url)) {
      if (!is.null(chunk_sizes)) {
        chunk_sizes$sizes <- c(chunk_sizes$sizes, file.size(req$body$data$media$path))
      }
      json_response(200, list(data = list(expires_at = 1700000000)))
    } else if (grepl("/media/upload/555/finalize$", url)) {
      data <- list(id = "555", media_key = "3_555", size = 10000)
      data$processing_info <- finalize_info
      json_response(200, list(data = data))
    } else if (grepl("/media/upload\\?", url)) {
      polls <<- polls + 1
      json_response(200, list(data = list(
        id = "555", media_key = "3_555", processing_info = states[[polls]]
      )))
    } else if (grepl("/media/metadata$", url)) {
      json_response(200, list(data = list(id = "555")))
    } else {
      stop("unexpected request to ", url)
    }
  }
}

# Records every wait instead of sleeping.
record_waits <- function(env = parent.frame()) {
  waits <- numeric(0)
  local_mocked_bindings(
    x_sleep = function(seconds) waits <<- c(waits, seconds),
    .package = "xapir", .env = env
  )
  function() waits
}

test_that("upload_media initializes, appends chunks in order and finalizes", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  path  <- fake_png()
  sizes <- new.env()
  seen  <- record_requests(media_responder(chunk_sizes = sizes))

  out <- withVisible(suppressMessages(
    upload_media(path, chunk_size = 3000)
  ))

  expect_false(out$visible)
  expect_equal(as.character(out$value), "555")
  expect_equal(attr(out$value, "media_key"), "3_555")

  reqs <- seen()
  # initialize, 4 chunks (3000 + 3000 + 3000 + 1000), finalize; no metadata
  expect_equal(length(reqs), 6)
  expect_true(all(vapply(reqs, function(r) r$method, "") == "POST"))
  expect_equal(reqs[[1]]$headers$Authorization, "Bearer tok")

  init <- reqs[[1]]
  expect_match(init$url, "^https://api.x.com/2/media/upload/initialize$")
  expect_equal(sent_json(init), list(
    media_type = "image/png", total_bytes = 10000, media_category = "tweet_image"
  ))

  appends <- reqs[2:5]
  for (i in seq_along(appends)) {
    expect_match(appends[[i]]$url, "/2/media/upload/555/append$")
    expect_equal(appends[[i]]$body$type, "multipart")
    expect_equal(appends[[i]]$body$data$segment_index, as.character(i - 1))
    expect_s3_class(appends[[i]]$body$data$media, "form_file")
  }
  expect_equal(sizes$sizes, c(3000, 3000, 3000, 1000))

  expect_match(reqs[[6]]$url, "/2/media/upload/555/finalize$")
  expect_null(reqs[[6]]$body)
})

test_that("upload_media guesses the media type from the extension", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  record_waits()
  seen  <- record_requests(media_responder())
  types <- c(jpg = "image/jpeg", JPEG = "image/jpeg", gif = "image/gif",
             webp = "image/webp", mp4 = "video/mp4", mov = "video/quicktime")
  for (ext in names(types)) {
    tmp <- withr::local_tempfile(fileext = paste0(".", ext))
    writeBin(as.raw(1:10), tmp)
    category <- switch(tolower(ext), mp4 = , mov = "tweet_video",
                       gif = "tweet_gif", "tweet_image")
    suppressMessages(upload_media(tmp, media_category = category))
    inits <- Filter(function(r) grepl("/initialize$", r$url), seen())
    init  <- sent_json(inits[[length(inits)]])
    expect_equal(init$media_type, unname(types[[ext]]))
    expect_equal(init$media_category, category)
    expect_equal(init$total_bytes, 10)
  }
})

test_that("upload_media stops before any request on a bad file or category", {
  httr2::local_mocked_responses(function(req) stop("a request was made"))
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")

  txt <- withr::local_tempfile(fileext = ".txt")
  writeBin(as.raw(1:10), txt)
  expect_error(upload_media(txt), "png, jpg, jpeg, gif, webp, mp4, mov")
  expect_error(upload_media("/no/such/file.png"), "No file found")

  mp4 <- withr::local_tempfile(fileext = ".mp4")
  writeBin(as.raw(1:10), mp4)
  expect_error(upload_media(mp4), "tweet_video")

  png <- fake_png()
  expect_error(upload_media(png, media_category = "tweet_video"), "mp4 or mov")
  expect_error(upload_media(png, media_category = "tweet_gif"), "gif files")
  expect_error(upload_media(png, chunk_size = 6 * 1024^2), "5 MB")
  expect_error(upload_media(png, alt_text = strrep("a", 1001)), "1,000 characters")
})

test_that("upload_media polls STATUS, waiting as long as the API asks", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  waits <- record_waits()
  path  <- fake_png()
  seen  <- record_requests(media_responder(
    finalize_info = list(state = "pending", check_after_secs = 2),
    states = list(
      list(state = "in_progress", check_after_secs = 5, progress_percent = 40),
      list(state = "succeeded", progress_percent = 100)
    )
  ))

  msgs <- character(0)
  out <- withCallingHandlers(
    upload_media(path),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_equal(as.character(out), "555")

  # finalize said wait 2, the first poll said wait 5, the second said done
  expect_equal(waits(), c(2, 5))

  reqs   <- seen()
  status <- Filter(function(r) grepl("command=STATUS", r$url), reqs)
  expect_equal(length(status), 2)
  expect_true(is.null(status[[1]]$method) || status[[1]]$method == "GET")
  expect_match(status[[1]]$url, "^https://api.x.com/2/media/upload\\?")
  expect_match(status[[1]]$url, "command=STATUS")
  expect_match(status[[1]]$url, "media_id=555")

  expect_true(any(grepl("^Uploading 1 chunk", msgs)))
  expect_true(any(grepl("^Processing", msgs)))
  expect_false(any(grepl("costs about", msgs)))
})

test_that("upload_media stops with the API's reason when processing fails", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  record_waits()
  path <- fake_png()
  record_requests(media_responder(
    finalize_info = list(state = "pending", check_after_secs = 1),
    states = list(list(
      state = "failed",
      error = list(code = 1, name = "InvalidMedia", message = "Unsupported video format.")
    ))
  ))
  expect_error(
    suppressMessages(upload_media(path)),
    "could not process the media: Unsupported video format."
  )
})

test_that("upload_media gives up after ten minutes of processing", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  waits <- record_waits()
  path  <- fake_png()
  record_requests(media_responder(
    finalize_info = list(state = "pending", check_after_secs = 250),
    states = list(
      list(state = "in_progress", check_after_secs = 250),
      list(state = "in_progress", check_after_secs = 250),
      list(state = "succeeded")
    )
  ))
  expect_error(suppressMessages(upload_media(path)), "10 minutes")
  # 250 + 250 = 500 s fits; a third wait would pass 600 s, so it stops.
  expect_equal(waits(), c(250, 250))
})

test_that("upload_media posts alt text once the media is ready", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  record_waits()
  path <- fake_png()
  seen <- record_requests(media_responder(
    finalize_info = list(state = "pending", check_after_secs = 1),
    states = list(list(state = "succeeded"))
  ))

  msgs <- character(0)
  withCallingHandlers(
    upload_media(path, alt_text = "A bar chart of sales by month"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  reqs <- seen()
  meta <- reqs[[length(reqs)]]
  expect_match(meta$url, "^https://api.x.com/2/media/metadata$")
  expect_equal(meta$method, "POST")
  expect_equal(sent_json(meta), list(
    id = "555", metadata = list(alt_text = list(text = "A bar chart of sales by month"))
  ))
  # the metadata request comes after the status poll that said succeeded
  expect_match(reqs[[length(reqs) - 1]]$url, "command=STATUS")
  expect_true(any(grepl("costs about \\$0.005", msgs)))
})

test_that("upload_media skips alt text and the metadata request when not asked", {
  local_mocked_bindings(authenticate_user = fake_token, .package = "xapir")
  path <- fake_png()
  seen <- record_requests(media_responder())
  suppressMessages(upload_media(path))
  urls <- vapply(seen(), function(r) r$url, "")
  expect_false(any(grepl("/media/metadata", urls)))
  expect_false(any(grepl("command=STATUS", urls)))
})
