#' Wrapper function to get all of the dates associated with the semester for a particular date or semester name
#' @param semesterName should be a semester name like "Ga" or "Zn". If no value is provided, semester information will be returned based on the date.
#' @param date should be a date of type character or Date in yyyy-mm-dd format. If semesterName is not provided, the function will return a data table with the dates associated with the semester relevant to the date provided.
#' @param moadConn should be an ODBC connection to MoAD; if not provided, it will connect to MoAD prod by default
#' @keywords semester, date
#' @export
#' @examples
#' chieR::dateMapping(startDate = as.Date("2023-01-01"), endDate = as.Date("2023-01-31"))
#'
semesterDates <- function(semesterName = NULL, date = Sys.Date(), moadConn = NULL){
  if(is.null(moadConn)){
    moadConn <- chieR::sqlConnect(server = "chiemoadprd") #"chiemoaddev"
  }

  if(is.null(semesterName)){
    date <- tryCatch({
      as.Date(date)
    }, error = function(e){
      print(e)
      stop("Unable to convert date in chieR::semesterDates() to Date type")
    })

    semesterName <- as.character(chieR::dateMapping(startDate = date, endDate = date, moadConn = moadConn)$Semester[1])

  }

  data <- chieR::runSQLQuery(moadConn, query = paste0("SELECT Date, element as Semester FROM dim.Date WHERE element='",
                                                      semesterName,
                                                      "'"))
  data[, Date := as.Date(Date)]
  setkey(data, Date)

  for(i in names(data)[!names(data) == "Date"]){
    data[, eval(i) := factor(get(i), levels = unique(get(i)))]
  }

  return(data[])
}
