#' Get font size
#' @param type should be "axisTitle", "axis", "legend", or "annotation"
#' @keywords font size
#' @export
#' @examples
#' getFontSize()
getFontSize <- function(type){
  if(!type %in% c("axisTitle", "axis", "legend", "annotation")){
    stop("Invalid input for type")
  }
  switch(type,
         axisTitle  = 12,
         axis       = 12,
         legend     = 12,
         annotation = 12)
}
