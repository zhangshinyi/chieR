% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agilityChart.R
\name{agilityPlot}
\alias{agilityPlot}
\title{Wrapper function for burndown plotly charts using Reynold's standard}
\usage{
agilityPlot(boxplotData, plotlyRangemode = NULL)
}
\arguments{
\item{boxplotData}{is a data table with two columns: Period and value (with a lower case v).}

\item{plotlyRangemode}{is used for rangemode in plotly::layout(). Can be useful to make it "nonnegative".}
}
\description{
Wrapper function for burndown plotly charts using Reynold's standard
}
\examples{
set.seed(1)
months <- c("Jan-23", "Feb-23", "Mar-23",  "Apr-23", "May-23")
data   <- data.table(Period = factor(rep(months, 100), levels = months),
                     value  = rnorm(500))
agilityPlot(data)
}
\keyword{agility,}
\keyword{boxplot}
\keyword{plot,}
