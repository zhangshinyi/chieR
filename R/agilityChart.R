#' Wrapper function for burndown plotly charts using Reynold's standard
#' @param boxplotData is a data table with two columns: Period and value (with a lower case v).
#' @param plotlyRangemode is used for rangemode in plotly::layout(). Can be useful to make it "nonnegative".
#' @keywords agility, plot, boxplot
#' @export
#' @examples
#' set.seed(1)
#' months <- c("Jan-23", "Feb-23", "Mar-23",  "Apr-23", "May-23")
#' data   <- data.table(Period = factor(rep(months, 100), levels = months),
#'                      value  = rnorm(500))
#' agilityPlot(data)
agilityPlot <- function(boxplotData, plotlyRangemode = NULL){
  boxplotData <- boxplotData[,
                             .(agility.minimum = round(min(value, na.rm = TRUE)),
                               agility.p_25    = round(quantile(value, na.rm = TRUE, probs = c(.25))),
                               agility.median  = round(median(value, na.rm = TRUE)),
                               agility.mean    = round(mean(value, na.rm = TRUE)),
                               agility.p_75    = round(quantile(value, na.rm = TRUE, probs = c(.75))),
                               agility.maximum = round(max(value, na.rm = TRUE))),
                             by = .(Period)]

  boxplot <- plot_ly(data       = boxplotData,
                     x          = ~Period,
                     type       = "box",
                     source     = "agilityPlot",
                     lowerfence = ~agility.minimum,
                     q1         = ~agility.p_25,
                     median     = ~agility.median,
                     q3         = ~agility.p_75,
                     upperfence = ~agility.maximum,
                     showlegend = FALSE) %>%
    add_markers(data       = boxplotData,
                x          = ~Period,
                y          = ~agility.mean,
                showlegend = FALSE,
                name       = "Average",
                marker     = list(size  = 10,
                                  color = "#000000")) %>%
    layout(yaxis  = list(rangemode = plotlyRangemode))
  chieR::plotlyLayout(boxplot)
}
