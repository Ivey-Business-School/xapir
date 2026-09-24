#' Upload Media
#'
#' @description
#' Uploads an image, GIF or video to X so it can be attached to a post, via
#' the [chunked upload
#' endpoints](https://docs.x.com/x-api/media/quickstart/media-upload-chunked).
#' Needs a user token, so the first call opens a browser window to sign in.
#'
#' Pass the returned id to [create_post()] as `media_ids`. A post can carry
#' up to 4 photos, 1 GIF or 1 video.
#'
#' @details
#' The upload runs in four steps, each printed as it happens: initialize
#' (tell X the type and size), append (send the file in chunks of at most
#' `chunk_size` bytes, so a large video is never read into memory in one
#' go), finalize, and, for a video or GIF, poll the status endpoint until X
#' has finished processing it. Processing usually takes a few seconds; the
#' function gives up after ten minutes with the media id so you can try
#' again later.
#'
#' The pricing page does not list media uploads, so no cost line is printed
#' for the upload itself. Alt text goes through the media metadata endpoint,
#' which is billed per request, and that request announces its cost.
#'
#' @importFrom httr2 req_body_json req_body_multipart req_method
#'   req_url_path_append req_url_query
#' @importFrom curl form_file
#' @param path Path to the file. The type is read from the extension: png,
#'   jpg, jpeg, gif, webp, mp4 or mov.
#' @param media_category What the file is for: `"tweet_image"` (the
#'   default) for a photo, `"tweet_video"` for a video, `"tweet_gif"` for an
#'   animated GIF. X limits size and length by category and by the
#'   account's tier.
#' @param alt_text Optional description of the image for screen readers, up
#'   to 1,000 characters. Sent as media metadata once the upload succeeds.
#' @param chunk_size Bytes per chunk. X accepts at most 5 MB a chunk; the
#'   default is 4 MB.
#' @return Invisibly, the media id as a string, with the `media_key` as an
#'   attribute. Stops with the API's message when a step is refused or
#'   processing fails.
#' @examples
#' \dontrun{
#' media_id <- upload_media("chart.png", alt_text = "Sales by month, 2026")
#' create_post("Our year so far", media_ids = media_id)
#'
#' video_id <- upload_media("launch.mp4", media_category = "tweet_video")
#' create_post("Watch the launch", media_ids = video_id)
#' }
#' @export
upload_media <- function(
  path,
  media_category = c("tweet_image", "tweet_video", "tweet_gif"),
  alt_text = NULL,
  chunk_size = 4 * 1024^2
) {

  media_category <- match.arg(media_category)
  media_type     <- guess_media_type(path)
  check_media_category(media_type, media_category)
  check_chunk_size(chunk_size)
  if (!is.null(alt_text)) {
    check_alt_text(alt_text)
  }

  total_bytes <- file.size(path)
  n_chunks    <- max(1, ceiling(total_bytes / chunk_size))

  token <- authenticate_user()
  base  <- x_request(token$access_token)

  # Step 1: initialize. X answers with the id every later step uses.
  message("Initializing upload of ", basename(path), " (",
          format_bytes(total_bytes), ")...")
  init <- base |>
    req_url_path_append("media", "upload", "initialize") |>
    req_method("POST") |>
    req_body_json(list(
      media_type     = media_type,
      total_bytes    = total_bytes,
      media_category = media_category
    )) |>
    x_perform()

  media_id  <- pluck(init, "data", "id")
  media_key <- pluck(init, "data", "media_key")
  if (is.null(media_id)) {
    stop("The API did not return a media id for the upload.", call. = FALSE)
  }

  # Step 2: append. The file is read one chunk at a time from an open
  # connection, so only one chunk is ever in memory. Each chunk goes out as
  # the `media` part of a multipart form, with its 0-based segment_index,
  # the way the docs' cURL example sends it.
  message("Uploading ", n_chunks, if (n_chunks == 1) " chunk..." else " chunks...")
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  segment_index <- 0
  repeat {
    chunk <- readBin(con, what = "raw", n = chunk_size)
    if (length(chunk) == 0) break
    append_chunk(base, media_id, chunk, segment_index, path)
    segment_index <- segment_index + 1
  }

  # Step 3: finalize. A video answers with processing_info and needs step 4.
  final <- base |>
    req_url_path_append("media", "upload", media_id, "finalize") |>
    req_method("POST") |>
    x_perform()
  media_key <- pluck(final, "data", "media_key") %||% media_key

  # Step 4: wait for processing when X asks.
  info <- pluck(final, "data", "processing_info")
  if (!is.null(info)) {
    wait_for_processing(base, media_id, info)
  }

  # Alt text is a separate, billed request, sent once the media is ready.
  if (!is.null(alt_text)) {
    message("Adding alt text...")
    announce_request_cost("media_metadata")
    base |>
      req_url_path_append("media", "metadata") |>
      req_method("POST") |>
      req_body_json(list(
        id       = media_id,
        metadata = list(alt_text = list(text = alt_text))
      )) |>
      x_perform()
  }

  message("Media ", media_id, " is ready to attach to a post.")
  invisible(structure(media_id, media_key = media_key))
}

# Sends one chunk. The chunk is written to a temporary file so curl can send
# it as a file part with a filename, which is how the docs upload chunks
# (`-F "media=@chunk"`). The temporary file is removed after the request.
append_chunk <- function(base, media_id, chunk, segment_index, path) {
  chunk_path <- tempfile("xapir-chunk-")
  on.exit(unlink(chunk_path), add = TRUE)
  writeBin(chunk, chunk_path)

  base |>
    req_url_path_append("media", "upload", media_id, "append") |>
    req_method("POST") |>
    req_body_multipart(
      media         = form_file(chunk_path, name = basename(path)),
      segment_index = as.character(segment_index)
    ) |>
    x_perform()

  invisible(NULL)
}

# Polls GET /2/media/upload?command=STATUS until the state is succeeded,
# waiting check_after_secs between polls as the API asks. Stops on a failed
# state with the API's reason, and after max_wait seconds in total.
wait_for_processing <- function(base, media_id, info, max_wait = 600) {
  message("Processing...")
  waited <- 0
  repeat {
    state <- info$state %||% "pending"
    if (identical(state, "succeeded")) {
      return(invisible(NULL))
    }
    if (identical(state, "failed")) {
      reason <- pluck(info, "error", "message") %||%
        pluck(info, "error", "name") %||% "X gave no reason."
      stop("X could not process the media: ", reason, call. = FALSE)
    }

    pause <- info$check_after_secs %||% 1
    if (waited + pause > max_wait) {
      stop(
        "X is still processing media ", media_id, " after ",
        round(max_wait / 60), " minutes. Try a smaller or shorter file, ",
        "or try again later.",
        call. = FALSE
      )
    }
    x_sleep(pause)
    waited <- waited + pause

    status <- base |>
      req_url_path_append("media", "upload") |>
      req_url_query(command = "STATUS", media_id = media_id) |>
      x_perform()
    info <- pluck(status, "data", "processing_info") %||%
      list(state = "succeeded")
  }
}

# A thin wrapper so tests can replace the wait without touching base.
x_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

# The MIME type the initialize step wants, read from the file extension.
guess_media_type <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("`path` must be one file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("No file found at \"", path, "\".", call. = FALSE)
  }
  name <- basename(path)
  ext  <- if (grepl(".", name, fixed = TRUE)) tolower(sub(".*\\.", "", name)) else ""
  types <- c(
    png  = "image/png",
    jpg  = "image/jpeg",
    jpeg = "image/jpeg",
    gif  = "image/gif",
    webp = "image/webp",
    mp4  = "video/mp4",
    mov  = "video/quicktime"
  )
  type <- types[ext]
  if (!nzchar(ext) || is.na(type)) {
    stop(
      "Cannot tell the media type of \"", basename(path), "\". ",
      "The file must end in one of: ",
      paste(names(types), collapse = ", "), ".",
      call. = FALSE
    )
  }
  unname(type)
}

# A video must be uploaded as tweet_video and a still image as tweet_image;
# a mismatch fails at X with a less helpful message.
check_media_category <- function(media_type, media_category) {
  is_video <- startsWith(media_type, "video/")
  is_gif   <- identical(media_type, "image/gif")
  if (is_video && !identical(media_category, "tweet_video")) {
    stop(
      "A video must be uploaded with media_category = \"tweet_video\".",
      call. = FALSE
    )
  }
  if (!is_video && identical(media_category, "tweet_video")) {
    stop(
      "media_category = \"tweet_video\" is for mp4 or mov files. ",
      "Use \"tweet_image\" for a photo or \"tweet_gif\" for an animated GIF.",
      call. = FALSE
    )
  }
  if (!is_gif && identical(media_category, "tweet_gif")) {
    stop(
      "media_category = \"tweet_gif\" is for gif files. ",
      "Use \"tweet_image\" for a photo.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

check_chunk_size <- function(chunk_size) {
  ok <- is.numeric(chunk_size) && length(chunk_size) == 1 &&
    !is.na(chunk_size) && chunk_size >= 1 && chunk_size <= 5 * 1024^2
  if (!ok) {
    stop(
      "`chunk_size` must be a number of bytes between 1 and 5 MB (",
      5 * 1024^2, "). X refuses larger chunks.",
      call. = FALSE
    )
  }
  invisible(chunk_size)
}

check_alt_text <- function(alt_text) {
  ok <- is.character(alt_text) && length(alt_text) == 1 &&
    !is.na(alt_text) && nzchar(alt_text) && nchar(alt_text) <= 1000
  if (!ok) {
    stop(
      "`alt_text` must be one string of at most 1,000 characters.",
      call. = FALSE
    )
  }
  invisible(alt_text)
}

# "9.8 KB", "4.0 MB": for the message that opens an upload.
format_bytes <- function(bytes) {
  if (bytes >= 1024^2) {
    sprintf("%.1f MB", bytes / 1024^2)
  } else if (bytes >= 1024) {
    sprintf("%.1f KB", bytes / 1024)
  } else {
    sprintf("%d bytes", as.integer(bytes))
  }
}
