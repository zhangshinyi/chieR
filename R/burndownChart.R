#' Wrapper function for burndown plotly charts using Reynold's standard
#' @param burndownBars is a data table with the bar values; requires the columns Period, lens, and value. Note that the variable column in the example is not required; a column to distinguish incoming/outgoing items is likely helpful (and a prerequisite for any aggregation done prior).
#' @param backlogLine is a data table with the backlog values for each period; requres the columns Period and Backlog.
#' @param horizontalLegend is a boolean value indicating whether the legend should be shown horizontally at the top or vertically on the right.
#' @keywords burndown, plotly, plot
#' @export
#' @examples
#' set.seed(1)
#' months <- c("Jan-23", "Feb-23", "Mar-23",  "Apr-23", "May-23")
#' data   <- rbind(data.table(Period   = factor(months, levels = months),
#'                            lens     = "Bucket 1",
#'                            variable = "Incoming",
#'                            value    = sample(0:30, 5, replace = FALSE)),
#'                 data.table(Period   = factor(months, levels = months),
#'                            lens     = "Bucket 2",
#'                            variable = "Incoming",
#'                            value    = sample(0:30, 5, replace = FALSE)),
#'                 data.table(Period   = factor(months, levels = months),
#'                            lens     = "Bucket 1",
#'                            variable = "Outgoing",
#'                            value    = -sample(0:30, 5, replace = FALSE)),
#'                 data.table(Period   = factor(months, levels = months),
#'                            lens     = "Bucket 2",
#'                            variable = "Outgoing",
#'                            value    = -sample(0:30, 5, replace = FALSE)))
#' priorQueue <- 50
#' totalLine  <- data[, .(Backlog = priorQueue + sum(value)), by = .(Period)]
#' burndownChart(burndownBars  = data,
#'               backlogLine  = totalLine)
burndownChart <- function(burndownBars, backlogLine, legendHeight = 1.05, horizontalLegend = NULL, sourceName = NULL, colorPalette = "Standard", showBacklog = TRUE){
  if(!is.factor(burndownBars$lens)){
    burndownBars <- copy(burndownBars)[, lens := factor(lens, levels = sort(unique(lens)))]
  }

  barTotals <- copy(burndownBars)[value != 0][, sign := sign(value)]
  barTotals <- barTotals[, .(value = sum(value)), by = .(Period, sign)]
  barTotals[, `:=`(text, format(round(value, 0), nsmall = 0, big.mark = ","))]

  lensValues <- sort(unique(burndownBars$lens))
  lensColors <- chieR::getColors(colorPalette)
  if(length(lensValues) <= length(lensColors)){
    lensColors <- lensColors[1:length(lensValues)]
  } else {
    lensColors <- lensColors[!lensColors %in% c("#2F2F2F", "#505050", "#737373", "#D2D2D2", "#E6E6E6", "#F2F2F2")]
    lensColors <- c(lensColors, gray.colors(length(lensValues) - length(lensColors)))
  }

  names(lensColors) <- lensValues

  plot <- plot_ly(source = sourceName) %>%
    add_bars(x      = ~burndownBars$Period,
             y      = ~burndownBars$value,
             color  = ~burndownBars$lens,
             colors = lensColors) %>%
    layout(barmode = "relative",
           xaxis   = list(title     = "",
                          tickangle = 320),
           yaxis   = list(title = ""))
  if(showBacklog){
    plot <- plot %>%
      add_markers(data   = backlogLine,
                  x      = backlogLine$Period,
                  y      = ~backlogLine$Backlog,
                  name   = "Backlog",
                  marker = list(color = "rgb(0, 0, 0)"),
                  line   = list(color = "rgb(0, 0, 0)",
                                dash  = "dash"))
  }
  plot <- plot %>%
    add_annotations(data      = barTotals,
                    x         = ~(as.numeric(Period) - 1),
                    y         = ~value,
                    text      = ~text,
                    xref      = "x",
                    yshift    = ~sign * 15,
                    showarrow = FALSE,
                    bgcolor   = "white",
                    opacity   = 0.85,
                    font      = list(family = chieR::getFont("Standard"),
                                     size  = chieR::getFontSize("annotation"))) %>%
    layout(margin     = list(l = 0),
           showlegend = TRUE)
  if(is.null(horizontalLegend)){
    horizontalLegend <- ifelse(length(lensValues) < 8, TRUE, FALSE)
  }
  chieR::plotlyLayout(plot, legendHeight = legendHeight, horizontalLegend = horizontalLegend)
}

