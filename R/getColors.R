#' Input a string indicating the type of color palette you want.
#' Options are Standard, Gen, HW Bucket, and Capability Gap
#' @param type should be "Standard", "Gen", "HW Bucket", or "Capability Gap"
#' @keywords colors
#' @export
#' @examples
#' getColors()
getColors <- function(type = "Standard"){
  if(!type %in% c("Standard", "Gen", "HW Bucket", "Capability Gap", "Processor")){
    stop("Invalid input for type")
  }
  colors <- switch(type,
                   "Standard" = c("#243A5E", "#0078D4", "#50E6FF", "#8661C5",
                                           "#D59DFF", "#6A4B16", "#FF9349", "#FFB900",
                                           "#054B16", "#107C10", "#9BF00B", "#274B47",
                                           "#008575", "#30E5D0", "#6B2929", "#FEF000",
                                           "#2F2F2F", "#505050", "#737373", "#D2D2D2",
                                           "#E6E6E6", "#F2F2F2"),
                                           "Gen"      = c("Gen3"  = "#50E6FF",
                                                          "Gen4"  = "#8661C5",
                                                          "Gen4M" = "#6A4B16",
                                                          "Gen5"  = "#FF9349",
                                                          "Gen6"  = "#FFB900",
                                                          "Gen6M" = "#11444B",
                                                          "Gen7"  = "#FFFB12",
                                                          "Gen7M" = "#6A4B16",
                                                          "Gen8"  = "#287380",
                                                          "Gen9"  = "#9BF00B"),
                   "Processor" = c("Intel" = "#243A5E",
                                   "AMD"   = "#0078D4",
                                   "Other" = "#50E6FF"),
                   "HW Bucket" = c("Disk"           = "#243A5E",
                                   "Memory"         = "#0078D4",
                                   "CPU"            = "#50E6FF",
                                   "Telemetry Gap"  = "#8661C5",
                                   "BIOS/BMC"       = "#D59DFF",
                                   "Motherboard"    = "#6A4B16",
                                   "GPU"            = "#FF9349",
                                   "SKU"            = "#054B16",
                                   "Networking"     = "#C3B7A2",
                                   "FPGA"           = "#107C10",
                                   "Rack Infra"     = "#A7B0BF",
                                   "TBD"            = "#FFB900",
                                   "PCIe"           = "#FEF000",
                                   "Buffer"         = "#D2B48C",
                                   "Capability Gap" = "#9BF00B",
                                   "Other"          = "#C3B7A2"),
                   "Capability Gap" = c("Live Migration at Scale"        = "#D83B01",
                                        "Disk Resiliency"                = "#505050",
                                        "OS Update"                      = "#A7BFB1",
                                        "Decomm"                         = "#D2D2D2",
                                        "Supplier Arch Limitation"       = "#30E5D0",
                                        "Blob Cache Resiliency"          = "#BF6E37",
                                        "Feature Development"            = "#EED8FF",
                                        "Predictive Failure Remediation" = "#FFE399",
                                        "Supplier Fix Update"            = "#7C899E"))
  return(colors)
}
