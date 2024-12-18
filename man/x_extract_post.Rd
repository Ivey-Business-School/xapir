% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/x_extract_post.R
\name{x_extract_post}
\alias{x_extract_post}
\title{Extract Post Data}
\usage{
x_extract_post(timeline, include_referenced_posts = TRUE)
}
\arguments{
\item{timeline}{A user's timeline.}

\item{include_referenced_posts}{TRUE if you want referenced posts, too.}
}
\value{
A \code{tibble} containing information about a post.
}
\description{
Extract a user's posts and referenced posts from a timeline object created
using \code{x_get_timeline()}.
}
\examples{
\dontrun{
tl <- x_read_timeline_post(timeline)
}
}
