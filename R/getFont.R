#' Get font type
#' @param type should be "Standard" or "Bold"
#' @keywords font
#' @export
#' @examples
#' getFont()
getFont <- function(type){
  if(!type %in% c("Standard", "Bold")){
    stop("Invalid input for type")
  }
  switch(type,
         Standard = "Segoe UI",
         Bold     = "Segoe UI Semibold")
}
