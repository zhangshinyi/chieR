#' SQL query for day, week, month formatting from dim.Date
#' @param startDate should be a date of type character or Date in yyyy-mm-dd format
#' @param endDate should be a date of type character or Date in yyyy-mm-dd format
#' @param moadConn should be an ODBC connection to MoAD; if not provided, it will connect to MoAD prod by default
#' @keywords date
#' @export
#' @examples
#' chieR::dateMapping(startDate = as.Date("2023-01-01"), endDate = as.Date("2023-01-31"))
#'
dateMapping <- function(startDate, endDate, moadConn = NULL){
  startDate <- tryCatch({
    as.Date(startDate)
  }, error = function(e){
    print(e)
    stop("Unable to convert startDate in dateMapping() to Date type")
  })
  endDate <- tryCatch({
    as.Date(endDate)
  }, error = function(e){
    print(e)
    stop("Unable to convert endDate in dateMapping() to Date type")
  })

  if(is.null(moadConn)){
    moadConn <- chieR::sqlConnect(server = "chiemoadprd") #"chiemoaddev"
  }

  dateQuery <- paste("SELECT Date, concat(outlookYear, ' ww', RIGHT('0' + CONVERT(VARCHAR(2), outlookWeek), 2)) as formattedWeek,",
                     "concat('FY', right(fiscalYear,2), fiscalQuarterName) as formattedQuarter,",
                     "concat(outlookYear, ' ww', RIGHT('0' + CONVERT(VARCHAR(2), outlookWeek), 2), '.', internationalDay) as formattedDay,",
                     "element as Semester",
                     "FROM dim.Date",
                     "WHERE date >= '%s' and date <='%s'",
                     "ORDER BY Date")

  data <- chieR::runSQLQuery(conn  = moadConn,
                             query = sprintf(dateQuery, startDate, endDate))[, Date := as.Date(Date)][, formattedMonth := format(Date, "%b-%y")]

  setkey(data, Date)

  return(data[])
}