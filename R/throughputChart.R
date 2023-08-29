#' Wrapper function for throughput plotly charts using Reynold's standard
#' @param throughputBars should be a data table with the column
#' @param semesterLineLocations should be a vector of numbers for where each semester line should be placed.
#' @keywords throughput, plotly, plot
#' @export
#' @examples
#' set.seed(1)
#' months  <- c("Oct-22", "Nov-22", "Dec-22", "Jan-23", "Feb-23", "Mar-23",  "Apr-23", "May-23")
#' nMonths <- length(months)
#' data    <- rbind(data.table(Period   = factor(months, levels = months),
#'                             lens     = "Bucket 1",
#'                             variable = "Incoming",
#'                             value    = sample(0:30, nMonths, replace = FALSE),
#'                             Semester = c(rep("Zn", 6), rep("Ga", 2))),
#'                  data.table(Period   = factor(months, levels = months),
#'                             lens     = "Bucket 2",
#'                             variable = "Incoming",
#'                             value    = sample(0:30, nMonths, replace = FALSE),
#'                             Semester = c(rep("Zn", 6), rep("Ga", 2))),
#'                  data.table(Period   = factor(months, levels = months),
#'                             lens     = "Bucket 1",
#'                             variable = "Outgoing",
#'                             value    = -sample(0:30, nMonths, replace = FALSE),
#'                             Semester = c(rep("Zn", 6), rep("Ga", 2))),
#'                  data.table(Period   = factor(months, levels = months),
#'                             lens     = "Bucket 2",
#'                             variable = "Outgoing",
#'                             value    = -sample(0:30, nMonths, replace = FALSE),
#'                             Semester = c(rep("Zn", 6), rep("Ga", 2))))
#' data[, Semester := factor(Semester, levels = c("Zn", "Ga"))]
#' setkey(data, variable, lens, Period, Semester)
#' data[, value := cumsum(value), by = .(Semester, variable, lens)]
#' # While hardcoded here, it is best if you use the level numbers for the periods based chieR::dateMapping(), and subtract 1.5.
#' semesterLine <- 5.5
#' throughputChart(data, semesterLineLocations = semesterLine)
throughputChart <- function(throughputBars, semesterLineLocations = NULL, sourceName = NULL){
  plot <- chieR::plotlyChartWrapper(data          = throughputBars,
                                    mode          = "bar",
                                    barmode       = "relative",
                                    showBarTotals = TRUE,
                                    totalsBySign  = TRUE,
                                    sourceName    = sourceName)
  if(!is.null(semesterLineLocations)){
    plot <- plot %>%
      layout(shapes = lapply(semesterLineLocations, chieR::vline))
  }
  plot
}
