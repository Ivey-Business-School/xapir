#' Extract Media Information from Timeline
#'
#' @description
#' Processes the timeline data retrieved from the X API to wrangle media information,
#' such as images, videos, and GIFs attached to posts.
#'
#' @importFrom purrr map map_dfr pluck flatten_chr compact
#' @importFrom dplyr mutate select any_of distinct
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
extract_post_media <- function(timeline) {

  post_media_map <- timeline |>
    map(pluck("data")) |>
    unlist(recursive = FALSE) |>
    map_dfr(
      ~ {
        keys <- .x$attachments$media_keys %||% NULL
        if (is.null(keys)) return(NULL)
        tibble(
          post_id   = .x$id,
          media_key = as.character(keys)  # ensure character vector
        )
      }
    ) |>
    unnest(media_key)

  # Extract media data directly
  timeline |>
    map(pluck("includes")) |>
    map(pluck("media")) |>
    unlist(recursive = FALSE) ->
    media_list

  # Define the variable order
  media_variables <-  c(
    "media_key",
    "type", 
    "view_count",
    "duration_ms", 
    "height", 
    "width", 
    "preview_image_url", 
    "url"
  )

  # Create the post media tibble
  media_list |>
    map_dfr(
      ~ tibble(
        media_key           = .x$media_key,
        type                = .x$type,
        view_count          = .x$public_metrics$view_count %||% NA |> as.character(),
        duration_ms         = .x$duration_ms %||% NA |> as.character(),
        height              = .x$height,
        width               = .x$width,
        preview_image_url   = .x$preview_image_url %||% NA |> as.character(),
        url                 = .x$url %||% 
          {
        variants <- .x$variants %||% list()
        first_mp4 <- purrr::pluck(
          purrr::detect(variants, ~ .x$content_type == "video/mp4"),
          "url",
          .default = NA_character_
        )
        first_mp4
        }
      )
    ) |>
    select(any_of(media_variables)) ->
    media_tbl

    # Join post_id to media table
    post_media <- media_tbl |>
      left_join(post_media_map, by = "media_key") |>
      select(post_id, any_of(c(
        "media_key", "type", "view_count", "duration_ms", "height",
        "width", "preview_image_url", "url"
      ))) |>
      distinct()

    return(post_media)
}
