% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iso_8601.R
\name{iso_8601}
\alias{iso_8601}
\title{Convert a Date or Date-Time to ISO 8601 UTC Format}
\usage{
iso_8601(x, tz = Sys.timezone())
}
\arguments{
\item{x}{A string (e.g., "2024-07-01" or "2024-07-01 12:30:00") or a date or
date-time object (Date, POSIXct).}

\item{tz}{A string specifying the input timezone (e.g., "America/New_York").
Defaults to \code{Sys.timezone()}, which uses the system's timezone if not
already present in \code{x}.}
}
\value{
A string in ISO 8601 UTC format.
}
\description{
This helper function takes a date or date-time input and converts it to
the required ISO 8601 format (\code{YYYY-MM-DDTHH:mm:ssZ}) in UTC. Users can
supply a date, a POSIXct object, or a string. If no time zone is specified,
the system's default timezone is assumed.
}
\examples{
\dontrun{
iso_8601("2024-07-01")
iso_8601("2024-07-01 15:00:00", tz = "America/New_York")
iso_8601(Sys.time())
}
}
