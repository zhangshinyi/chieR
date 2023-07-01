#' Wrapper function for creating vertical lines with plotly::layout() shapes
#' @param x is the x-axis location of the vertical line.
#' @keywords vline, vertical, line
#' @export
#' @examples
vline <- function(x){
  list(type = "line",
       y0   = 0,
       y1   = 1,
       yref = "paper",
       x0   = x,
       x1   = x,
       line = list(color = "black",
                   dash  = "dash"))
}
