#' Standard for agility charts
#' @param type must be agility, trend, burndown, or throughput
#' @keywords standard, wrapper, inputs
#' @export
#' @examples
standardInputs <- function(data,
                           filterCols,
                           type,
                           naToUnknownTF = FALSE,
                           moadConn      = chieR::sqlConnect(server = "chiemoadprd"),
                           semesterDates = chieR::semesterDates(moadConn = moadConn)[]){

  type <- tolower(type)
  validTypes <- c("agility", "burndown", "throughput", "trend")
  if(!type %in% validTypes){
    stop("Date column required")
  }

  if(type == "agility"){
    if(!"value" %in% names(data)){
      stop("value (with lower case v) column required")
    }
  } else if(type %in% c("burndown", "throughput")){
    if(!"Type" %in% names(data)){
      stop("Type column required; should have the values Incoming or Outgoing")
    }

    if(!identical(c("Incoming", "Outgoing"),
                  sort(as.character(unique(data$Type))))){
      stop("Invalid values in Type column; restricted to Incoming and Outgoing")
    }

    if(!"Count" %in% names(data)){
      data[, Count := fifelse(Type == "Incoming", 1, -1)]
    }
  } else if(type == "trend"){
    if(!"Count" %in% names(data)){
      data[, Count := 1]
    }
  }

  if(!"Date" %in% names(data)){
    stop("Date column required")
  }

  if(class(data$Date) != "Date"){
    data[, Date := as.Date(Date, tryFormats = c("%Y-%m-%d", "%m/%d/%y", "%m/%d/%Y"))]
  }

  data <- data[!is.na(Date)]

  dateMapping <- chieR::dateMapping(startDate = min(data$Date, na.rm = TRUE),
                                    endDate   = max(semesterDates$Date),
                                    moadConn  = moadConn)

  if(naToUnknownTF){
    data <- chieR::naToUnknown(copy(data),
                               filterCols = filterCols)
  }

  list(data          = data,
       filterCols    = filterCols,
       semesterDates = semesterDates,
       dateMapping   = dateMapping)
}
