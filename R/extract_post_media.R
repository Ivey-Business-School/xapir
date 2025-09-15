#' Extract Media Information from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle media information,
#' such as images, videos, and GIFs attached to posts.
#'
#' @importFrom purrr map map_dfr pluck detect
#' @importFrom dplyr select any_of distinct left_join
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @param timeline A list containing the timeline data retrieved from the X API.
#' @return A tibble containing structured media data.
#' @examples
#' \dontrun{
#' timeline <- get_timeline(
#'   username = "XDevelopers",
#'   max_results = 100,
#'   start_time = iso_8601(Sys.Date() - 7)
#' )
#' post_mediat <- extract_post_media(timeline)
#' }
#' @export
extract_post_media <- function(
  timeline
) {

  media_variables <-  c(
    "post_id", "media_id", "type", "view_count", "duration_ms", "height", 
    "width", "preview_image_url", "url", "bit_rate"
  )

  post_media_map <- timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        keys <- .x$attachments$media_keys %||% NULL
        if (is.null(keys)) return(NULL)
        tibble(
          post_id   = .x$id,
          media_id  = as.character(keys)  # ensure character vector
        )
      }
    )

  if (nrow(post_media_map) == 0) {
    return(tibble(!!!setNames(rep(list(logical(0)), length(media_variables)), media_variables)))
  }

  post_media_map <- unnest(post_media_map, media_id)

  # Extract media data directly
  timeline |>
    map(pluck("includes")) |>
    map(pluck("media")) |>
    unlist(recursive = FALSE) ->
    media_list
  
  if (length(media_list) == 0) {
    return(tibble(!!!setNames(rep(list(logical(0)), length(media_variables)), media_variables)))
  }

  # Create the post media tibble
  media_list |>
  map_dfr(
    ~ {
      variants <- .x$variants %||% list()
      first_mp4 <- detect(variants, ~ .x$content_type == "video/mp4")

      tibble(
        media_id          = .x$media_key,
        type              = .x$type,
        view_count        = .x$public_metrics$view_count %||% NA_integer_,
        duration_ms       = .x$duration_ms %||% NA_integer_,
        height            = .x$height,
        width             = .x$width,
        preview_image_url = .x$preview_image_url %||% NA |> as.character(),
        url               = first_mp4$url %||% .x$url %||% NA_character_,
        bit_rate          = first_mp4$bit_rate %||% NA_integer_
      )
    }
  ) |>
    select(any_of(media_variables[-1])) ->
    media_tbl

    # Join post_id to media table
    post_media <- media_tbl |>
      left_join(post_media_map, by = "media_id") |>
      select(any_of(media_variables)) |>
      distinct()

    return(post_media)
}
