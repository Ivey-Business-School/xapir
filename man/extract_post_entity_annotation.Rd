% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_post_entity_annotation.R
\name{extract_post_entity_annotation}
\alias{extract_post_entity_annotation}
\title{Extract Post Entity Annotation Data from Timeline}
\usage{
extract_post_entity_annotation(timeline)
}
\arguments{
\item{timeline}{A list containing the timeline data retrieved from the X API.}
}
\value{
A tibble containing structured post context data.
}
\description{
Processes the timeline data retrieved from the X API to wrangle post entity annotation
data, including metadata relating to the type and its probability.
}
\examples{
\dontrun{
timeline <- get_timeline(
  username = "XDevelopers",
  max_results = 100,
  start_time = iso_8601(Sys.Date() - 7)
)
post <- extract_post_entity_annotation(timeline)
}
}
