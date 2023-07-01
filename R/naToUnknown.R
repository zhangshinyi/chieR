#' Wrapper function to change NA values to "Unknown". Important because filtering will ignore NA values.
#' @param data is a data table
#' @param filterCols is a vector of column names in data
#' @param charReplace is the string replacement for NAs.
#' @param numReplace is the numeric replacement for NAs.
#' @keywords filter, NA, Unknown
#' @export
#' @examples
naToUnknown <- function(data, filterCols, charReplace = "Unknown", numReplace = -999){
  filterCols <- filterCols[filterCols %in% names(data)]
  if(length(filterCols) > 0){
    filterColClass <- data.table(filterCols = filterCols,
                                 class = sapply(filterCols, function(i){ class(data[[i]]) }))

    charCols <- filterColClass[class == "character"]$filterCols
    if(length(charCols) > 0){
      data <- data[, (charCols) := lapply(.SD, function(i){ i[is.na(i)] <- charReplace; i }), .SDcols = charCols]
    }

    numCols  <- filterColClass[class %in% c("integer", "numeric")]$filterCols
    if(length(numCols) > 0){
      data <- data[, (numCols)  := lapply(.SD, function(i){ i[is.na(i)] <- numReplace; i }),  .SDcols = numCols]
    }
  }
  data
}
