#' Wrapper function to change NA values to "Unknown". Important because filtering will ignore NA values.
#' @param x is the x-axis location of the vertical line.
#' @keywords vline, vertical, line
naToUnknown <- function(data, filterCols){
  data[, (filterCols) := lapply(.SD, function(i){ i[is.na(i)] <- "Unknown"; i }), .SDcols = filterCols]
}
