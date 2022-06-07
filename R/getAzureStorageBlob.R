#' Get blob from Azure Storage
#' @param storageAccount Storage account name
#' @param accountKey Account key
#' @param container Container
#' @param filename File name
#' @keywords storage
#' @export
#' @examples
#' getAzureStorageBlob()
getAzureStorageBlob <- function(storageAccount, accountKey, container, filename){
  endp         <- AzureStor::storage_endpoint(paste0("https://", storageAccount, ".blob.core.windows.net"), 
                                              key = accountKey)
  cont         <- AzureStor::storage_container(endp, container)
  tempfilename <- tempfile()
  if(blob_exists(cont, filename)){
    print(paste0(filename, " file exists!"))
    AzureStor::storage_download(cont, filename, tempfilename)
    data         <- data.table::fread(tempfilename)
    return(data)
  } else {
    print(paste0("File doesn't exist at: ", Sys.time()))
    return(NULL)
  }
}