% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runSQLQuery.R
\name{runSQLQuery}
\alias{runSQLQuery}
\title{SQL query to run given connection and (table name or full query)}
\usage{
runSQLQuery(conn, table = NULL, dateColumn = NULL, columns = "*", query = NULL)
}
\arguments{
\item{conn}{should be be a connection (e.g., a call of chieR::sqlConnect())}

\item{table}{is the table name. Must provide table or query.}

\item{dateColumn}{is a column that should be converted to type Date if applicable}

\item{columns}{is a vector of column names if you don't want to pull all columns by default}

\item{query}{is the full query if you don't want to deal with the other arguments. Must provide table or query.}
}
\description{
SQL query to run given connection and (table name or full query)
}
\examples{
runSQLQuery()
}
\keyword{query}
