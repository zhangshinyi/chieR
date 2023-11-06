#' Standard for burndown charts
#' @param input is passed from Shiny server
#' @param output is passed from Shiny server
#' @param session is passed from Shiny server
#' @keywords filters
#' @export
#' @examples
burndownStandard <- function(input, output, session, burndownData,
                             keyword,
                             boxTitle                      = "Burndown",
                             showTable                     = FALSE,
                             addlTableColumns              = NULL,
                             burndownActionButtonGroupList = NULL,
                             actionButtonInputFunction     = NULL,
                             filterDefaultSelectedValues   = NULL,
                             defaultLens                   = "None"){

  burndownWithActionButtonFilter <- reactive({
    if(!is.null(actionButtonInputFunction) & !is.null(burndownActionButtonGroupList)){
      data <- actionButtonInputFunction(dataList   = burndownData,
                                        inputValue = input[[burndownActionButtonGroupList$inputId]])
    } else {
      data <- copy(burndownData$data)
    }
    data
  })

  filterCols <- reactive({
    shiny::req(burndownData$filterCols)
    filters <- burndownData$filterCols
    data.table(col       = filters,
               inputName = paste0(keyword, gsub(" ", "", filters)))
  })

  output[[paste0(keyword, "UI")]] <- renderUI({
    shiny::req(is.logical(showTable))
    if(showTable){
      tableOutput <- DT::dataTableOutput(paste0(keyword, "Table"))
    } else {
      tableOutput <- NULL
    }
    if(!is.null(burndownActionButtonGroupList)){
      burndownActionButtonGroup <- radioGroupButtons(inputId  = burndownActionButtonGroupList$inputId,
                                                     label    = NULL,
                                                     choices  = burndownActionButtonGroupList$choices,
                                                     status   = "primary")
    } else {
      burndownActionButtonGroup <- NULL
    }
    list(fluidRow(column(width = 12,
                         div(style = "display:inline-block; float:right",
                             tipify(actionButton(paste0("reset", keyword, "Input"),
                                                 "",
                                                 icon = icon("redo")),
                                    "Refresh settings",
                                    placement = "top"))),
                  column(width = 12,
                         uiOutput(paste0("resetable", keyword, "Input")))),
         fluidRow(
           column(width = 12,
                  box(width       = NULL,
                      title       = boxTitle,
                      status      = "primary",
                      solidHeader = TRUE,
                      burndownActionButtonGroup,
                      plotlyOutput(paste0(keyword, "Plot"),
                                   height = "590px"),
                      tableOutput,
                      align = "center")
           )))
  })

  output[[paste0("set", keyword, "DateRange")]] <- renderUI({
    shiny::req(burndownData$semesterDates)

    # minDate <- min(burndownData$semesterDates$Date)#min(burndownWithActionButtonFilter()$Date)
    maxDate <- max(burndownData$semesterDates$Date)

    semesterStartDate <- min(burndownData$semesterDates$Date)

    dateRangeInput(paste0(keyword, "DateRange"),
                   label = "Date Range",
                   # min   = minDate,
                   start = seq(semesterStartDate, length = 2, by = "-3 months")[2],
                   end   = maxDate,
                   # max   = maxDate,
                   width = "100%")
  })

  output[[paste0("resetable", keyword, "Input")]] <- renderUI({
    shiny::req(burndownData$data, filterCols())
    filterWidgets <- mapply(function(csvColName, inputName){
      inputChoices <- as.character(sort(unique(burndownData$data[[csvColName]])))
      if(!is.null(filterDefaultSelectedValues)){
        if(csvColName %in% names(filterDefaultSelectedValues)){
          selectedChoices <- filterDefaultSelectedValues[[csvColName]]
        } else {
          selectedChoices <- inputChoices
        }
      } else {
        selectedChoices <- inputChoices
      }

      column(width = 2,
             pickerInput(inputName,
                         csvColName,
                         selected = selectedChoices,
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
                                            choices  = c("None", burndownData$filterCols),
                                            selected = defaultLens,
                                            multiple = FALSE)))

    div(id = paste0(keyword, input[[paste0("reset", keyword, "Input")]]),
        chieR::organizeWidgets(filters = filterWidgets, dateLensWidgets = fixedWidgets, 3))
  })

  burndownFiltered <- reactive({
    shiny::req(burndownWithActionButtonFilter())
    data  <- copy(burndownWithActionButtonFilter())

    for(i in 1:nrow(filterCols())){
      shiny::req(input[[filterCols()[i]$inputName]])
      data <- data[get(filterCols()[i]$col) %in% input[[filterCols()[i]$inputName]]]
    }

    data
  })

  burndownPeriodMapping <- reactive({
    shiny::req(input[[paste0(keyword, "TimeInterval")]],
               burndownData$dateMapping)

    periodCol   <- paste0("formatted", input[[paste0(keyword, "TimeInterval")]])
    dateMapping <- copy(burndownData$dateMapping)[, c("Date", periodCol), with = FALSE]
    setnames(dateMapping, periodCol, "Period")
    dateMapping
  })

  burndownDatePeriod <- reactive({
    shiny::req(burndownFiltered(), nrow(burndownFiltered()) > 0,
               burndownPeriodMapping())
    merge(copy(burndownFiltered()),
          burndownPeriodMapping(),
          by    = "Date",
          all.x = TRUE)
  })

  burndownLensAggregated <- reactive({
    shiny::req(burndownDatePeriod(), nrow(burndownDatePeriod()) > 0,
               input[[paste0(keyword, "Lens")]],
               input[[paste0(keyword, "DateRange")]])
    data <- copy(burndownDatePeriod())[(Date >= min(input[[paste0(keyword, "DateRange")]])) & (Date <= max(input[[paste0(keyword, "DateRange")]]))]

    if(input[[paste0(keyword, "Lens")]] != "None"){
      data <- data[, .(Count = sum(Count)), by = c("Period", "Type", input[[paste0(keyword, "Lens")]])]
    } else {
      data <- data[, .(Count = sum(Count)), by = c("Period", "Type")][, None := "None"]
    }

    setnames(data, input[[paste0(keyword, "Lens")]], "lens")

    data <- dcast(data, Period + lens ~ Type, value.var = "Count", fill = 0)

    backlog <- copy(burndownDatePeriod())[, .(Backlog = sum(Count)), by = Period][, Backlog := cumsum(Backlog)]

    list(inOut   = data,
         backlog = backlog)
  })

  burndownDateFilter <- reactive({
    shiny::req(burndownLensAggregated()$inOut, nrow(burndownLensAggregated()$inOut) > 0,
               input[[paste0(keyword, "DateRange")]],
               input[[paste0(keyword, "TimeInterval")]])
    # Filter for valid periods
    dateMapping  <- copy(burndownPeriodMapping())[(Date >= min(input[[paste0(keyword, "DateRange")]])) & (Date <= max(input[[paste0(keyword, "DateRange")]]))]
    data         <- lapply(copy(burndownLensAggregated()), function(i){ i[Period %in% dateMapping$Period] })

    # Fill in 0s for missing period/lens combinations
    periodLensCross <- data.table(tidyr::expand_grid(Period = unique(dateMapping$Period),
                                                     lens   = unique(data$inOut$lens)))
    data$inOut <- merge(periodLensCross, data$inOut, by = c("Period", "lens"), all.x = TRUE)
    data$inOut[is.na(Incoming), Incoming := 0]
    data$inOut[is.na(Outgoing), Outgoing := 0]
    setkey(data$inOut, Period, lens)

    return(data)
  })

  output[[paste0(keyword, "Plot")]] <- renderPlotly({
    shiny::req(burndownDateFilter()$inOut, nrow(burndownDateFilter()$inOut) > 0,
               burndownDateFilter()$backlog)
    chieR::burndownChart(burndownBars = melt(copy(burndownDateFilter()$inOut), c("Period", "lens"))[variable %in% c("Incoming", "Outgoing")],
                         backlogLine  = copy(burndownDateFilter()$backlog),
                         sourceName   = paste0(keyword, "Plot"))
  })

  burndown            <- reactiveValues()
  burndown$clickTable <- FALSE

  burndownWithPeriod <- reactive({
    shiny::req(burndownFiltered(), burndownPeriodMapping())
    merge(copy(burndownFiltered()), burndownPeriodMapping(), by = "Date", all.x = TRUE)
  })

  # If user clicks, filter more as appropriate
  observeEvent(event_data("plotly_click", source = paste0(keyword, "Plot")), {
    shiny::req(burndownWithPeriod())
    burndown$clickTable <- TRUE
    clickInfo          <- event_data("plotly_click", source = paste0(keyword, "Plot"))
    burndown$tableData  <- copy(burndownWithPeriod())[Period %in% clickInfo$x]
  })

  # If user clicks, filter more as appropriate
  observeEvent(input[[paste0("reset", keyword, "Input")]], {
    burndown$clickTable <- FALSE
  })

  output[[paste0(keyword, "Table")]] <- DT::renderDataTable({
    shiny::req(burndownWithPeriod(), nrow(burndownWithPeriod()) > 0,
               is.logical(burndown$clickTable))

    if(!burndown$clickTable){
      tableData <- copy(burndownWithPeriod())
    } else {
      tableData <- copy(burndown$tableData)
    }

    tableData <- tableData[, c("Date", addlTableColumns, burndownData$filterCols), with = FALSE]

    DT::datatable(copy(tableData),
                  options = list(lengthMenu = c(20, 50),
                                 scrollX    = TRUE,
                                 server     = TRUE))
  })
}
