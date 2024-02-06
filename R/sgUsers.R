#' Given one or more security group IDs, return a vector of valid user IDs
#' @param sgIds is a vector of security IDs
#' @keywords security, group
#' @export
#' @examples
#' sgUsers("06132b8c-7940-4244-a032-389382b4fb64")
sgUsers <- function(sgIds){
  data <- rbindlist(lapply(sgIds, function(sgId){
    print(sgId)
    URL <- sprintf("https://schiesgreader.azurewebsites.net/api/SCHIESGReader?code=dFIVFM3HYyQ8iJDbTPprQDKElrcccAfaD0Yr6cKPw7zCAzFulPDmLA==&sgId=%s",
                   sgId)
    fromJSON(content(GET(url = URL),
                     "text",
                     encoding = "UTF-8"),
             flatten = TRUE)[]
  }))
  identifiedIndividuals <- data[!is.na(userPrincipalName)]$userPrincipalName
  remainingGroups       <- data[is.na(userPrincipalName)]
  if(nrow(remainingGroups) > 0){
    return(c(identifiedIndividuals, sgUsers(remainingGroups$id)))
  } else {
    return(identifiedIndividuals)
  }
}
