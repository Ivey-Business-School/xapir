% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_post_media.R
\name{extract_post_media}
\alias{extract_post_media}
\title{Extract Media Information from Timeline}
\usage{
extract_post_media(timeline)
}
\arguments{
\item{timeline}{A list containing the timeline data retrieved from the X API.}
}
\value{
A tibble containing structured media data.
}
\description{
Processes the timeline data retrieved from the X API to wrangle media information,
such as images, videos, and GIFs attached to posts.
}
\examples{
\dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post_mediat <- extract_post_media(timeline)
}
}
