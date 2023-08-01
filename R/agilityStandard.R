#' Standard for agility charts
#' @param input is passed from Shiny server
#' @param output is passed from Shiny server
#' @param session is passed from Shiny server
#' @keywords filters
#' @export
#' @examples
agilityStandard <- function(input, output, session, agilityData, keyword){ 
  
  ###################################################################################### UI, Widgets
  
  filterCols <- reactive({
    filters <- agilityData$filterCols
    data.table(col       = filters,
               inputName = paste0(keyword, gsub(" ", "", filters)))
  })
  
  output[[paste0("set", keyword, "DateRange")]] <- renderUI({
    shiny::req(agilityData$data)
    
    dates <- copy(agilityData$data)$Date
    
    minDate   <- min(dates)
    maxDate   <- max(dates)
    
    dateRangeInput(paste0(keyword, "DateRange"), 
                   label = "Date Range",
                   min   = minDate,
                   start = max(c(seq(min(agilityData$semesterDates$Date), length = 2, by = "-3 months")[2], minDate)),
                   end   = maxDate,
                   max   = maxDate,
                   width = "100%")
    #)
  })
  
  output[[paste0("resetable", keyword, "Input")]] <- renderUI({
    shiny::req(agilityData$data, filterCols())
    filterWidgets <- mapply(function(csvColName, inputName){
      inputChoices <- as.character(sort(unique(agilityData$data[[csvColName]])))
      column(width = 2,
             pickerInput(inputName,
                         csvColName, 
                         selected = inputChoices,
                         choices  = inputChoices,
                         options  = list(`actions-box`          = TRUE, 
                                         `live-search`          = TRUE,
                                         `selected-text-format` = paste0("count > ", length(inputChoices) - 1),
                                         `count-selected-text`  = "All"),
                         multiple = TRUE))
    },
    csvColName = filterCols()$col,
    inputName  = filterCols()$inputName,
    SIMPLIFY   = FALSE)
    
    fixedWidgets <- list(column(width = 2,
                                pickerInput(paste0(keyword, "TimeInterval"),
                                            label    = "Time Interval",
                                            choices  = c("Week", "Month"), 
                                            selected = "Month",
                                            multiple = FALSE)),
                         column(width = 2,
                                uiOutput(paste0("set", keyword, "DateRange"))),
                         column(width = 2,
                                pickerInput(paste0(keyword, "Lens"),
                                            label    = "Lens",
                                            choices  = "None", 
                                            selected = "None",
                                            multiple = FALSE)))
    
    div(id = paste0(keyword, input[[paste0("reset", keyword, "Input")]]),
        chieR::organizeWidgets(filters = filterWidgets, dateLensWidgets = fixedWidgets, 3))
  })
  
  ###################################################################################### Calcs
  
  # Filter based on UI widgets
  agilityFiltered <- reactive({
    shiny::req(agilityData$data, 
               input[[paste0(keyword, "DateRange")]])
    
    data  <- copy(agilityData$data)
    
    for(i in 1:nrow(filterCols())){
      shiny::req(input[[filterCols()[i]$inputName]])
      data <- data[get(filterCols()[i]$col) %in% input[[filterCols()[i]$inputName]]]
    }
    
    # Filter by date range
    data <- data[(Date >= min(input[[paste0(keyword, "DateRange")]])) & (Date <= max(input[[paste0(keyword, "DateRange")]]))]
    
    data
  })
  
  agilityPeriodMapping <- reactive({
    shiny::req(input[[paste0(keyword, "TimeInterval")]])
    periodCol   <- paste0("formatted", input[[paste0(keyword, "TimeInterval")]])
    dateMapping <- copy(agilityData$dateMapping)[, c("Date", periodCol), with = FALSE]
    setnames(dateMapping, periodCol, "Period")
    dateMapping
  })
  
  agilityWithPeriod <- reactive({
    shiny::req(agilityFiltered(), agilityPeriodMapping())
    merge(copy(agilityFiltered()), agilityPeriodMapping(), by = "Date", all.x = TRUE)
  })
  
  agilityAggregated <- reactive({
    shiny::req(agilityWithPeriod(), nrow(agilityWithPeriod()) > 0, 
               input[[paste0(keyword, "DateRange")]],
               agilityPeriodMapping())
    
    data <- copy(agilityWithPeriod())[, .(Period, value)]
    
    # Fill in missing periods if needed
    missingPeriods  <- unique(agilityPeriodMapping()[(Date >= min(input[[paste0(keyword, "DateRange")]])) & 
                                                       (Date <= max(input[[paste0(keyword, "DateRange")]])) & 
                                                       !(Period %in% data$Period)]$Period)
    if(length(missingPeriods) > 0){
      data <- rbind(data,
                    data.table(Period = missingPeriods,
                               value  = -100))
    }
    
    data
  })
  
  ###################################################################################### Plot
  
  output[[paste0(keyword, "Plot")]] <- renderPlotly({
    shiny::req(agilityAggregated(), nrow(agilityAggregated()) > 0)
    chieR::agilityPlot(boxplotData     = copy(agilityAggregated()), 
                       plotlyRangemode = "nonnegative") %>%
      layout(margin = list(l = 0))
  })
  
  ###################################################################################### Table
  
  agility            <- reactiveValues()
  agility$clickTable <- FALSE
  
  # If user clicks, filter more as appropriate
  observeEvent(event_data("plotly_click", source = paste0(keyword, "Plot")), {
    shiny::req(agilityWithPeriod())
    agility$clickTable <- TRUE
    clickInfo          <- event_data("plotly_click", source = paste0(keyword, "Plot"))
    agility$tableData  <- copy(agilityWithPeriod())[Period %in% clickInfo$x]
  })
  
  # If user clicks, filter more as appropriate
  observeEvent(input$resetagilityInput, {
    agility$clickTable <- FALSE
  })
  
  output[[paste0(keyword, "Table")]] <- renderReactable({
    shiny::req(agilityWithPeriod(), nrow(agilityWithPeriod()) > 0, 
               is.logical(agility$clickTable))
    
    if(!agility$clickTable){
      tableData <- copy(agilityWithPeriod())
    } else {
      tableData <- copy(agility$tableData)
    }
    
    # Add HTML for URL
    tableData[, Id := paste0('<a href="', Url, '" target="_blank" rel="noopener noreferrer">', Id,'</a>')]
    
    tableData <- tableData[, c("Id",
                               "Start Date",
                               "End Date",
                               "Title",
                               "DevModel",
                               "Severity",
                               "Scenario",
                               "ClusterSize",
                               "value",
                               "SLA"), with = FALSE]
    setnames(tableData, 
             c("ClusterSize",  "DevModel",  "value"),
             c("Cluster Size", "Dev Model", "Duration"))
    
    tableData <- tableData[!is.na(Duration)]
    
    tableData[, `SLA Met` := ifelse(Duration <= SLA, "Yes", "No")]
    
    setkey(tableData, Id)
    
    reactable(tableData,
              defaultColDef   = colDef(vAlign      = "center",
                                       header      = function(value){ gsub(".", " ", value, fixed = TRUE) },
                                       cell        = function(value){ format(value, nsmall = 0) },
                                       align       = "center",
                                       html        = TRUE,
                                       minWidth    = 20,
                                       headerStyle = list(background = "#f7f7f8")),
              columns         = list(`Title`   = colDef(width = 500),
                                     `SLA Met` = colDef(
                                       cell = function(value) {
                                         if(value  == "No") shiny::icon("times-circle", class = "fas",  
                                                                        style = "color: #D83B01") else shiny::icon("check-circle", class = "fas",  
                                                                                                                   style = "color: #107C10")
                                       }
                                     )),
              striped         = TRUE, 
              bordered        = TRUE,
              highlight       = TRUE)
  })
}