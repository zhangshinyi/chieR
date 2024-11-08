#' SQL query to run given connection and (table name or full query)
#' @param conn should be be a connection (e.g., a call of chieR::sqlConnect())
#' @param table is the table name. Must provide table or query.
#' @param dateColumn is a column that should be converted to type Date if applicable
#' @param columns is a vector of column names if you don't want to pull all columns by default
#' @param query is the full query if you don't want to deal with the other arguments. Must provide table or query.
#' @keywords query
#' @export
#' @examples
#' runSQLQuery()
runSQLQuery <- function(conn, table = NULL, dateColumn = NULL, columns = "*", query = NULL){

  if(is.null(table) & is.null(query)){
    abort("Must provide either table name or full SQL query")
  }

  if(is.null(query)){
    if(!identical("*", columns)){
      columns <- paste(columns, collapse = ",")
    }

    query <- sprintf(paste("SELECT", columns, "FROM %s"), table)
  }

  print("chieR::runSQLQuery() query:")
  print(conn)
  print(query)
  data <- data.table::data.table(RODBC::sqlQuery(conn, query))

  if(!is.null(dateColumn)){
    data[, eval(dateColumn) := as.Date(get(dateColumn), "%m/%d/%Y")]
    setkeyv(data, dateColumn)
  }

  print(paste0("Data dimensions: [", paste(dim(data), collapse = ", "), "]"))

  print("Data preview:")
  print(head(data))

  return(data[])
}
