#' Wrapper function for predictability plotly charts using Reynold's standard
#' @param data should be a data table in wide format with the columns Period and Predictability, and at least one other column that would be represented with bars.
#' @param minBarLabel is the minimum value for which bar labels should be shown; assumed to be 1, but this can be crowded.
#' @param barAxisHeight Height of axis for bars; optional.
#' @param predictabilityLineName Name of line as shown in legend. Assumed by default to be Predictability.
#' @param sourceName source to be passed to plot_ly() in preparation for linking with event_data()
#' @param legendHeight Adjust the height of the legend if necessary
#' @keywords predictability, plotly, plot
#' @export
#' @examples
#' set.seed(1)
#' periods <- c("Jan-23", "Feb-23", "Mar-23", "Apr-23", "May-23", "Jun-23")
#' data <- data.table(Period  = factor(periods, levels = periods),
#'                    `Bar 1` = round(runif(length(periods), 1, 15)),
#'                    `Bar 2` = round(runif(length(periods), 1, 15)),
#'                    Predictability = runif(length(periods), 1, 100) / 100)
#' predictabilityChart(data                   = copy(data),
#'                     predictabilityLineName = "Cycle Time")
predictabilityChart <- function(data,
                                minBarLabel            = 1,
                                barAxisHeight          = NULL,
                                predictabilityLineName = "Predictability",
                                sourceName             = NULL,
                                legendHeight           = NULL){

  # Assume that anything other than the Period and Predictability columns is intended to be a bar
  barLevels <- names(data)[!names(data) %in% c("Period", "Predictability")]

  barData <- melt(data[, c("Period", barLevels), with = FALSE], "Period")
  setkey(barData, Period, variable)

  barData[, labelHeight := (cumsum(value) - value / 2), by = Period]

  barColors        <- chieR::getColors()[1:length(barLevels)]
  names(barColors) <- barLevels

  fig <- plot_ly(source = sourceName) %>%
    add_bars(x           = ~barData$Period,
             y           = ~barData$value,
             name        = ~barData$variable,
             type        = "bar",
             yaxis       = "y2",
             color       = ~barData$variable,
             # opacity     = .4,
             # showlegend  = FALSE,
             colors      = barColors,
             # legendgroup = "Bars",
             orientation = "v") %>%
    layout(barmode = "relative",
           yaxis2   = list(title    = "",
                           showgrid = FALSE,
                           overlaying = "y",
                           side     = "left"))

  # Set bar axis height
  if(is.null(barAxisHeight)){
    dataByPeriod  <- merge(barData[, .(barTotal = sum(value)), by = Period],
                           data[, .(Period, Predictability)],
                           by = "Period")[, axisHeight := (barTotal / Predictability)]
    barAxisHeight <- 10 + ceiling(max(dataByPeriod[!is.infinite(axisHeight) & !is.nan(axisHeight)]$axisHeight)/10) * 10
  }
  fig <- fig %>% layout(yaxis2 = list(range = c(0, barAxisHeight)))

  # Add line for predictability
  predictabilityLineColor <- chieR::getColors()[2]
  fig <- fig %>%
    add_trace(x           = ~data$Period,
              y           = ~data$Predictability,
              # yaxis       = "y2",
              name        = predictabilityLineName,
              mode        = "lines+markers",
              # legendgroup = "Lines",
              line        = list(color = predictabilityLineColor),
              marker      = list(color = predictabilityLineColor),
              type        = "scatter")

  # Add bar/line labels
  fig <- fig %>%
    add_annotations(x         = ~data[!is.nan(Predictability)]$Period,
                    y         = ~data[!is.nan(Predictability)]$Predictability,
                    text      = ~paste0(gsub(" ", "", format(round(100*data[!is.nan(Predictability)]$Predictability, 2), nsmall = 2)), "%"),
                    xref      = "x",
                    yref      = "y",
                    yshift    = 15,
                    showarrow = FALSE,
                    bgcolor   = "white",
                    opacity   = .85,
                    font      = list(family = chieR::getFont("Standard"),
                                     size   = chieR::getFontSize("annotation"))) %>%
    add_annotations(x         = ~barData[value >= minBarLabel]$Period,
                    y         = ~barData[value >= minBarLabel]$labelHeight,
                    text      = ~barData[value >= minBarLabel]$value,
                    xref      = "x",
                    yref      = "y2",
                    showarrow = FALSE,
                    bgcolor   = "white",
                    opacity   = .85,
                    font      = list(family = chieR::getFont("Standard"),
                                     size   = chieR::getFontSize("annotation")))

  # Predictability axis formatting
  fig <- fig %>%
    layout(yaxis  = list(title      = "",
                         tickformat = ".0%",
                         showgrid   = FALSE,
                         range      = c(0, 1.05),
                         # overlaying = "y",
                         side       = "right"))

  # Legend orientation
  legendItems <- length(barLevels) + 1
  if(legendItems < 8){
    horizontalLegend <- TRUE
  } else {
    horizontalLegend <- FALSE
  }

  chieR::plotlyLayout(fig, horizontalLegend = horizontalLegend, legendHeight = legendHeight) %>%
    plotly::config(displayModeBar = FALSE) %>%
    layout(xaxis  = list(title = ""))
}
