#' Connect to MoAD, other databases.
#' @param server should be "chiemoaddev", "chiemoadprd", or "dsireport"
#' @param authentication should be "ActiveDirectoryServicePrincipal", "ActiveDirectoryMsi"
#' @keywords connection, sql, MoAD
#' @export
#' @examples
#' sqlConnect()
sqlConnect <- function(server, authentication = "ActiveDirectoryMsi", token = NULL, clientID = NULL, clientSecret = NULL){
  if(!server %in% c("chiemoaddev", "dsireport", "chiemoadprd")){
    stop("Invalid input for server")
  }

  if(!authentication %in% c("ActiveDirectoryServicePrincipal", "ActiveDirectoryMsi")){
    stop("Invalid input for authentication")
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

  server <- paste0(server, serverSuffix)

  if(is.null(clientID) & authentication == "ActiveDirectoryServicePrincipal"){
    clientID <- AzureKeyVault::key_vault(idURL)$secrets$get(tail(strsplit(idURL, "/")[[1]], 1))$value
  }
  if(is.null(clientSecret) & authentication == "ActiveDirectoryServicePrincipal"){
    clientSecret <- AzureKeyVault::key_vault(secretURL)$secrets$get(tail(strsplit(secretURL, "/")[[1]], 1))$value
  }

  if(authentication == "ActiveDirectoryMsi"){
    connectionStringSQL <- sprintf(paste0("Driver={ODBC Driver 18 for SQL Server};",
                                          "Server=tcp:%s,1433;",
                                          "Database=%s;",
                                          "Encrypt=yes;",
                                          "TrustServerCertificate=no;",
                                          "Connection Timeout=9999;",
                                          "Authentication=%s"),
                                   server,
                                   database,
                                   authentication)
  } else if(authentication == "ActiveDirectoryServicePrincipal"){
    connectionStringSQL <- sprintf(paste0("Driver={ODBC Driver 18 for SQL Server};",
                                          "Server=tcp:%s,1433;",
                                          "Database=%s;",
                                          "UID=%s;",
                                          "PWD=%s;",
                                          "Encrypt=yes;",
                                          "TrustServerCertificate=no;",
                                          "Connection Timeout=9999;",
                                          "Authentication=%s"),
                                   server,
                                   database,
                                   clientID,
                                   clientSecret,
                                   authentication)
  }
  print(paste0("sqlConnect() connection string for server ", server, ":"))
  print(connectionStringSQL)

  conn <- RODBC::odbcDriverConnect(connectionStringSQL)

  return(conn)
}
