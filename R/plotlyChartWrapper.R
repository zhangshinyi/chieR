#' Wrapper function for line/bar plotly charts using Reynold's standard
#' @param data should be a data table in long format (i.e., melted; see example for details) with the columns Period, lens (if there's no lens, set lens to be the same value for every row), and value. The Order column is optional; if it exists, it should be unique to each Period value.
#' @param mode should be "bar" or "lines"
#' @param barmode should be options like "group", "relative", or "stacked"
#' @param colorPalette should be acceptable types for chieR::getColor()
#' @keywords line, bar, plotly, plot
#' @export
#' @examples
#' data <- data.table(WorldPhones)[, Period := 2001:2007][, Order := 1:7][]
#' data <- melt(data, c("Period", "Order"), variable.name = "lens")
#'
#' plotlyChartWrapper(data,
#'                    mode            = "lines",
#'                    yaxisLabel      = "Number of telephones")
#'
#' plotlyChartWrapper(data,
#'                    mode            = "bar",
#'                    barmode         = "relative",
#'                    yaxisLabel      = "Number of telephones",
#'                    showBarTotals   = TRUE)
#'
#' plotlyChartWrapper(data,
#'                    mode            = "bar",
#'                    barmode         = "group",
#'                    yaxisLabel      = "Number of telephones")
plotlyChartWrapper <- function(data, mode,
                               barmode         = "relative",
                               colorPalette    = "Standard",
                               yaxisLabel      = "",
                               showBarTotals   = FALSE,
                               barTotalDecimal = 0,
                               percent         = FALSE,
                               legendHeight    = 1.1){
  if(!mode %in% c("bar", "lines")){
    stop("Invalid input for mode")
  }

  if(!barmode %in% c("group", "relative", "stacked")){
    stop("Invalid input for barmode")
  }

  if("Order" %in% names(data)){
    periodLevels <- unique(data[, Period, Order])[order(Order)]$Period
    data[, Order := NULL]
  } else {
    periodLevels <- unique(data$Period)
  }
  data[, Period := factor(Period, levels = periodLevels)]

  if(mode == "lines"){
    type <- "scatter"
  } else if(mode == "bar"){
    type <- NULL
  }

  lensValues        <- sort(unique(data$lens))
  lensColors        <- chieR::getColors(colorPalette)[1:length(lensValues)]
  names(lensColors) <- lensValues

  output <- plot_ly(data,
                    x        = ~Period,
                    y        = ~value,
                    color    = ~lens,
                    colors   = lensColors,
                    type     = type,
                    mode     = mode) %>%
    plotly::layout(barmode = barmode,
                   legend  = list(traceorder = "normal"))

  if(showBarTotals &
     (mode == "bar")){
    barTotals <- copy(data)[, .(value = sum(value)), by = Period]
    barTotals[, text := format(round(ifelse(percent, 100, 1) * value, barTotalDecimal),
                               nsmall   = barTotalDecimal,
                               big.mark = ",")]
    output <- output %>%
      add_annotations(data = barTotals,
                      x         = ~(as.numeric(Period) - 1),
                      y         = ~value,
                      text      = ~text,
                      xref      = "x",
                      yshift    = 15,
                      showarrow = FALSE,
                      bgcolor   = "white",
                      opacity   = .85,
                      font      = list(family = chieR::getFont("Standard"),
                                       size   = chieR::getFontSize("annotation")))
  }

  chieR::plotlyLayout(output,
                      yaxisLabel       = yaxisLabel,
                      horizontalLegend = ifelse(length(lensValues) < 8, TRUE, FALSE),
                      legendHeight     = legendHeight)
}
