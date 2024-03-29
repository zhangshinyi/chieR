% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sgUsers.R
\name{sgUsers}
\alias{sgUsers}
\title{Given one or more security group IDs, return a vector of valid user IDs}
\usage{
sgUsers(sgIds, secret = NULL)
}
\arguments{
\item{sgIds}{is a vector of security IDs}
}
\description{
Given one or more security group IDs, return a vector of valid user IDs
}
\examples{
sgUsers("06132b8c-7940-4244-a032-389382b4fb64")
}
\keyword{group}
\keyword{security,}
