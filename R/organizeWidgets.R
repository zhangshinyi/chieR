#' Show automated filters, fixed widgets (e.g., for time interval, date range, lens) based on the standard
#' @param filters are the automated widgets
#' @param dateLensWidgets are the date/lens-related widgets
#' @keywords filters
#' @export
#' @examples
organizeWidgets <- function(filters, dateLensWidgets, maxrow1filters = 3){
  row1NumFilters <- min(c(maxrow1filters, length(filters)))
  row1widgets <- filters[1:row1NumFilters]
  if(row1NumFilters != maxrow1filters){
    row1widgets <- list(row1widgets, column(width = (maxrow1filters * 2 - row1NumFilters * 2)))
  }
  row1widgets <- fluidRow(c(row1widgets, dateLensWidgets))
  if(length(filters) > maxrow1filters){
    row2widgets <- fluidRow(filters[(maxrow1filters + 1):length(filters)])
  } else {
    row2widgets <- NULL
  }
  list(row1widgets,
       row2widgets)
}