#' Get CLI token
#' @param resource (e.g., https://storage.azure.com)
#' @keywords cli, token
#' @export
#' @examples
#' cliToken(resource = "https://storage.azure.com")
#'
cliToken <- function(resource){
  call <- paste("az account get-access-token",
                "--resource ",
                resource,
                "--query accessToken --output tsv")
  token <- system(call, intern = TRUE)
  token
}
