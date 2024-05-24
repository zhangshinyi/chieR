#' Connect to MoAD, other databases.
#' @param server should be "chiemoaddev", "chiemoadprd", or "dsireport"
#' @keywords connection
#' @export
#' @examples
#' sqlConnect()
sqlConnect <- function(server, token = NULL, managedID = NULL, clientID = NULL, clientSecret = NULL){
  if(!server %in% c("chiemoaddev", "dsireport", "chiemoadprd")){
    stop("Invalid input for server")
  }
  if(is.null(managedID)){
    managedID <- ifelse(Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect", TRUE, FALSE)
  }
  if(server %in% c("chiemoaddev", "chiemoadprd")){
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
  if(is.null(clientID)){
    clientID <- AzureKeyVault::key_vault(idURL,
                                         token               = token,
                                         as_managed_identity = managedID)$secrets$get(tail(strsplit(idURL, "/")[[1]], 1))$value
  }
  if(is.null(clientSecret)){
    clientSecret <- AzureKeyVault::key_vault(secretURL,
                                             token               = token,
                                             as_managed_identity = managedID)$secrets$get(tail(strsplit(secretURL, "/")[[1]], 1))$value
  }
  connectionStringSQL <- sprintf("Driver={ODBC Driver 18 for SQL Server};Server=tcp:%s,1433;Database=%s;UID=%s;PWD=%s;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=15;Authentication=ActiveDirectoryServicePrincipal",
                                 server,
                                 database,
                                 clientID,
                                 clientSecret)
  print("sqlConnect() connection string:")
  print(connectionStringSQL)
  conn                <- RODBC::odbcDriverConnect(connectionStringSQL)
  return(conn)
}
