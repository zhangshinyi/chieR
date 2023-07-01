#' Wrapper function to change NA values to "Unknown". Important because filtering will ignore NA values.
#' @param data is a data table
#' @param filterCols is a vector of column names in data
#' @keywords filter, NA, Unknown
#' @export
#' @examples
naToUnknown <- function(data, filterCols){
  filterCols <- filterCols[filterCols %in% names(data)]
  if(length(filterCols) > 0){
    data <- data[, (filterCols) := lapply(.SD, function(i){ i[is.na(i)] <- "Unknown"; i }), .SDcols = filterCols]
  }
  data
}
