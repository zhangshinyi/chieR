#' Plotly defaults that conform to CHIE standard
#' @param plot is a plotly object
#' @param xaxisLabel is a string
#' @param yaxisLabel is a string
#' @param horizontalLegend Boolean for whether the legend should be shown at the top or on the right
#' @param legendHeight Legends at the top have height issues, so set this to some appropriate value > 1 (e.g., 1.25)
#' @keywords plotly
#' @export
#' @examples
#' plotlyLayout()
plotlyLayout <- function(plot, 
                         xaxisLabel       = "", 
                         yaxisLabel       = "", 
                         horizontalLegend = TRUE, 
                         legendHeight     = 1){
  plot <- plot %>% layout(yaxis   = list(title       = list(text = yaxisLabel,
                                                            font = list(size   = chieR::getFontSize("axisTitle"), 
                                                                        family =  chieR::getFont("Standard"))),
                                         tickfont    = list(family = chieR::getFont("Standard"),
                                                            size   = chieR::getFontSize("axis"))),
                          xaxis   = list(title = list(text = xaxisLabel,
                                                      font = list(size   = chieR::getFontSize("axisTitle"), 
                                                                  family =  chieR::getFont("Standard"))),
                                         tickangle = 320,
                                         tickfont  = list(family = chieR::getFont("Standard"),
                                                          size   = chieR::getFontSize("axis"))))
  if(horizontalLegend){
    plot <- plot %>% layout(legend = list(orientation = "h",
                                          xanchor     = "center",
                                          yanchor     = "top",
                                          x           = 0.5,
                                          y           = legendHeight,
                                          font        = list(family = chieR::getFont("Standard"),
                                                             size   = chieR::getFontSize("legend"))))
  } else {
    plot <- plot %>% layout(legend = list(y           = .5,
                                          font        = list(family = chieR::getFont("Standard"),
                                                             size   = chieR::getFontSize("legend"))))
  }
  return(plot)
}