#' Given one or more security group IDs, return a vector of valid user IDs
#' @param sgIds is a vector of security IDs
#' @keywords security, group
#' @export
#' @examples
#' sgUsers("06132b8c-7940-4244-a032-389382b4fb64")
sgUsers <- function(sgIds, secret = NULL){
  if(is.null(secret)){
    # Get secret from key vault
    secret <- key_vault("https://schie-chatbot-vault.vault.azure.net/secrets/azfunc-code-vault/bd046283ff1f4a869368fca9009ea706",
                        as_managed_identity = (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect"))$secrets$get("azfunc-code-vault")$value
  }

  # Paste together URL and secret
  urlPrefix <- paste0("https://schiesgreader.azurewebsites.net/api/SCHIESGReader?code=",
                      secret)
  data <- rbindlist(lapply(sgIds, function(sgId){
    print(sgId)
    URL <- sprintf(paste0(urlPrefix,
                          "&sgId=%s"),
                   sgId)
    fromJSON(content(GET(url = URL),
                     "text",
                     encoding = "UTF-8"),
             flatten = TRUE)[]
  }))
  identifiedIndividuals <- data[!is.na(userPrincipalName)]$userPrincipalName
  remainingGroups       <- data[is.na(userPrincipalName)]
  if(nrow(remainingGroups) > 0){
    return(c(identifiedIndividuals, sgUsers(remainingGroups$id, secret = secret)))
  } else {
    return(identifiedIndividuals)
  }
}
