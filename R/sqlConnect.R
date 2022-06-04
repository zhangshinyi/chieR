#' Connect to MoAD, other databases.
#' @param server should be "chiemoaddev" or "dsireport"
#' @keywords connection
#' @export
#' @examples
#' sqlConnect()
sqlConnect <- function(server){
  if(!server %in% c("chiemoaddev", "dsireport")){
    stop("Invalid input for server")
  }
  managedID    <- ifelse(Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect", TRUE, FALSE)
  if(server == "chiemoaddev"){
    idURL        <- "https://moaddev6131880268.vault.azure.net/secrets/dev-synapse-sqlpool-sp-id/"
    secretURL    <- "https://moaddev6131880268.vault.azure.net/secrets/dev-synapse-sqlpool-sp-secret/"
    database     <- "moad_sql"
    serverSuffix <- ".sql.azuresynapse.net"
  } else if(server == "dsireport"){
    idURL        <- "https://chmedsisqltokens.vault.azure.net/secrets/chmedsisqluserappclientid/"
    secretURL    <- "https://chmedsisqltokens.vault.azure.net/secrets/chmedsirshinyclientsecret/"
    database     <- "DSIAutomation"
    serverSuffix <- ".database.windows.net"
  }
  server              <- paste0(server, serverSuffix)
  clientID            <- key_vault(idURL, as_managed_identity = managedID)$secrets$get(tail(strsplit(idURL, "/")[[1]], 1))$value
  clientSecret        <- key_vault(secretURL, as_managed_identity = managedID)$secrets$get(tail(strsplit(secretURL, "/")[[1]], 1))$value
  connectionStringSQL <- sprintf("Driver={ODBC Driver 18 for SQL Server};Server=tcp:%s,1433;Database=%s;UID=%s;PWD=%s;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=15;Authentication=ActiveDirectoryServicePrincipal",
                                 server,
                                 database,
                                 clientID,
                                 clientSecret)
  conn                <- odbcDriverConnect(connectionStringSQL)
  return(conn)
}
