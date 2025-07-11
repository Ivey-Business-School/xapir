% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/follow_user.R
\name{follow_user}
\alias{follow_user}
\title{Follow User}
\usage{
follow_user(source_username, target_username)
}
\arguments{
\item{source_username}{Username of account that will follow someone.}

\item{target_username}{Username of account that will be followed.}
}
\description{
Causes the User(in the path) to follow, or “request to follow” for protected Users, the target User
via the \href{https://docs.x.com/x-api/users/follow-user}{follow user endpoint}.
The User(in the path) must match the User context authorizing the request
}
\examples{
\dontrun{
follow_user(source_username = "Tesla", target_username = "elonmusk")
}
}
