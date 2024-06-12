#' Get managed identity token
#' @param resource (e.g., https://storage.azure.com)
#' @keywords managed, identity, token
#' @export
#' @examples
#' miToken(resource = "https://storage.azure.com")
#'
miToken <- function(resource){
  api_version <- "2019-08-01"
  mi_endpoint <- Sys.getenv("MSI_ENDPOINT")
  mi_secret   <- Sys.getenv("MSI_SECRET")
  headers     <- c(`X-IDENTITY-HEADER` = mi_secret)

  ## Fetch managed identity token
  res      <- GET(url = paste0(mi_endpoint, '?resource=', resource, "&api-version=", api_version), add_headers(.headers = headers))
  mi_token <- content(res)$access_token

  mi_token
}
