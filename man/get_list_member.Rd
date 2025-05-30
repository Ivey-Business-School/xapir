% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_list_member.R
\name{get_list_member}
\alias{get_list_member}
\title{Get List Members}
\usage{
get_list_member(list_id, client_id = Sys.getenv("X_CLIENT_ID"))
}
\arguments{
\item{list_id}{A string representing the unique ID of the list.}

\item{client_id}{A string containing the OAuth 2.0 client ID for authenticating
with the X API. By default, this argument retrieves the token from the
environment variable \code{X_CLIENT_ID} (via \code{Sys.getenv("X_CLIENT_ID")}).
Adding your client ID to your \code{.Renviron} file ensures it is securely
stored and accessible without needing to manually input it for each
session.}
}
\value{
A tibble with user details (ID, name, username) of list members, or NULL if none found.
}
\description{
Returns a list of Users that are members of a List by the provided List ID via the \href{https://docs.x.com/x-api/users/returns-user-objects-that-are-members-of-a-list-by-the-provided-list-id}{get list members}.
}
\examples{
\dontrun{
list_members <- get_list_members(list_ID)
}
}
