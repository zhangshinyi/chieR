#' Standard for throughput charts
#' @param input is passed from Shiny server
#' @param output is passed from Shiny server
#' @param session is passed from Shiny server
#' @keywords filters
#' @export
#' @examples
throughputStandard <- function(input, output, session, throughputData, keyword,
                               boxTitle                        = "Throughput",
                               showTable                       = FALSE,
                               addlTableColumns                = NULL,
                               throughputActionButtonGroupList = NULL,
                               actionButtonInputFunction       = NULL,
                               cumulateBySemester              = TRUE,
                               filterDefaultSelectedValues     = NULL,
                               defaultLens                     = "None"){
  throughputWithActionButtonFilter <- reactive({
    if(!is.null(actionButtonInputFunction) & !is.null(throughputActionButtonGroupList)){
      data <- actionButtonInputFunction(dataList   = throughputData,
                                        inputValue = input[[throughputActionButtonGroupList$inputId]])
    } else {
      data <- copy(throughputData$data)
    }
    data
  })

  output[[paste0(keyword, "UI")]] <- renderUI({
    shiny::req(is.logical(showTable))
    if(showTable){
      tableOutput <- DT::dataTableOutput(paste0(keyword, "Table"))
    } else {
      tableOutput <- NULL
    }
    if(!is.null(throughputActionButtonGroupList)){
      throughputActionButtonGroup <- radioGroupButtons(inputId  = throughputActionButtonGroupList$inputId,
                                                       label    = NULL,
                                                       choices  = throughputActionButtonGroupList$choices,
                                                       status   = "primary")
    } else {
      throughputActionButtonGroup <- NULL
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
                      throughputActionButtonGroup,
                      plotlyOutput(paste0(keyword, "Plot"),
                                   height = "590px"),
                      tableOutput,
                      align = "center")
           )))
  })

  filterCols <- reactive({
    shiny::req(throughputData$filterCols)
    filters <- throughputData$filterCols
    data.table(col       = filters,
               inputName = paste0(keyword, gsub(" ", "", filters)))
  })

  output[[paste0("set", keyword, "DateRange")]] <- renderUI({
    shiny::req(throughputWithActionButtonFilter(), throughputData$semesterDates)

    minDate <- min(throughputWithActionButtonFilter()$Date)
    maxDate <- max(throughputData$semesterDates$Date)

    semesterStartDate <- min(throughputData$semesterDates$Date)

    dateRangeInput(paste0(keyword, "DateRange"),
                   label = "Date Range",
                   min   = minDate,
                   start = max(c(minDate, seq(semesterStartDate, length = 2, by = "-3 months")[2])),
                   end   = maxDate,
                   max   = maxDate,
                   width = "100%")
  })

  output[[paste0("resetable", keyword, "Input")]] <- renderUI({
    shiny::req(throughputWithActionButtonFilter(), filterCols())
    filterWidgets <- mapply(function(csvColName, inputName){
      inputChoices <- as.character(sort(unique(throughputWithActionButtonFilter()[[csvColName]])))
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
                                            choices  = c("None", throughputData$filterCols),
                                            selected = defaultLens,
                                            multiple = FALSE)))

    div(id = paste0(keyword, input[[paste0("reset", keyword, "Input")]]),
        chieR::organizeWidgets(filters = filterWidgets, dateLensWidgets = fixedWidgets, 3))
  })

  throughputFiltered <- reactive({
    shiny::req(throughputWithActionButtonFilter(), input[[paste0(keyword, "DateRange")]])

    dateRange <- input[[paste0(keyword, "DateRange")]]
    data      <- copy(throughputWithActionButtonFilter())[(Date >= min(dateRange)) & (Date <= max(dateRange))]

    for(i in 1:nrow(filterCols())){
      shiny::req(input[[filterCols()[i]$inputName]])
      data <- data[get(filterCols()[i]$col) %in% input[[filterCols()[i]$inputName]]]
    }

    data
  })

  throughputPeriodDateMapping <- reactive({
    shiny::req(input[[paste0(keyword, "TimeInterval")]],
               throughputData$dateMapping)
    periodCol   <- paste0("formatted", input[[paste0(keyword, "TimeInterval")]])
    dateMapping <- copy(throughputData$dateMapping)[, c("Date", periodCol, "Semester"), with = FALSE]
    setnames(dateMapping, periodCol, "Period")
    if(!cumulateBySemester){
      dateMapping[, Semester := "N/A"]
    }

    maxThroughputDate <- max(throughputWithActionButtonFilter()$Date)
    dateMapping[, Actual := fifelse(Period %in% dateMapping[Date <= maxThroughputDate]$Period,
                                    1,
                                    0)]
    dateMapping
  })

  throughputDatePeriod <- reactive({
    shiny::req(throughputFiltered(), nrow(throughputFiltered()) > 0,
               throughputPeriodDateMapping())
    merge(copy(throughputFiltered()), copy(throughputPeriodDateMapping()), by = "Date", all.x = TRUE)
  })

  throughputLensAggregated <- reactive({
    shiny::req(throughputDatePeriod(), nrow(throughputDatePeriod()) > 0,
               input[[paste0(keyword, "Lens")]])

    data <- copy(throughputDatePeriod())

    lens <- input[[paste0(keyword, "Lens")]]

    if(lens != "None"){
      data <- data[, .(Count = sum(Count)), by = c("Period", "Semester", "Type", lens)]
    } else {
      data <- data[, .(Count = sum(Count)), by = c("Period", "Semester", "Type")][, None := "None"]
    }
    setnames(data, lens, "lens")

    periodLensTypeCross <- data.table(Reduce(tidyr::expand_grid, list(unique(throughputPeriodDateMapping()[, c("Semester", "Period"), with = FALSE]),
                                                                      data.table(lens = unique(data$lens)),
                                                                      data.table(Type = unique(data$Type)))))

    data <- merge(periodLensTypeCross, data, all.x = TRUE, by = c("Semester", "Period", "lens", "Type"))[is.na(Count), Count := 0]
    setkey(data, Type, lens, Semester, Period)

    # Cumulative sum across semester
    data <- dcast(data[, Count := cumsum(Count), by = .(Semester, lens, Type)],
                  lens + Semester + Period ~ Type,
                  value.var = "Count",
                  fill      = 0)[, Semester := NULL]

    data
  })

  throughputDateFilter <- reactive({
    shiny::req(throughputLensAggregated(), nrow(throughputLensAggregated()) > 0,
               input[[paste0(keyword, "DateRange")]])

    dateRange <- input[[paste0(keyword, "DateRange")]]

    # Filter by date range
    dateRangePeriods <- throughputPeriodDateMapping()[(Date >= min(dateRange)) & (Date <= max(dateRange))]$Period
    data             <- copy(throughputLensAggregated())[Period %in% dateRangePeriods]

    # Zero out future months
    futurePeriods <- unique(throughputPeriodDateMapping()[Actual == 0]$Period)
    data[Period %in% futurePeriods, Incoming := 0]
    data[Period %in% futurePeriods, Outgoing := 0]
    data
  })

  output[[paste0(keyword, "Plot")]] <- renderPlotly({
    chieR::throughputChart(throughputBars        = melt(copy(throughputDateFilter()), c("Period", "lens"))[variable %in% c("Incoming", "Outgoing")],
                           semesterLineLocations = NULL,
                           sourceName            = paste0(keyword, "Plot"))
  })

  throughput            <- reactiveValues()
  throughput$clickTable <- FALSE

  throughputWithPeriod <- reactive({
    shiny::req(throughputFiltered(), throughputPeriodDateMapping())
    merge(copy(throughputFiltered()), throughputPeriodDateMapping(), by = "Date", all.x = TRUE)
  })

  # If user clicks, filter more as appropriate
  observeEvent(event_data("plotly_click", source = paste0(keyword, "Plot")), {
    browser()
    shiny::req(throughputWithPeriod())
    throughput$clickTable <- TRUE
    clickInfo             <- event_data("plotly_click", source = paste0(keyword, "Plot"))
    throughput$tableData  <- copy(throughputWithPeriod())[Period %in% clickInfo$x]
  })

  # If user clicks, filter more as appropriate
  observeEvent(input[[paste0("reset", keyword, "Input")]], {
    throughput$clickTable <- FALSE
  })

  output[[paste0(keyword, "Table")]] <- DT::renderDataTable({
    shiny::req(throughputWithPeriod(), nrow(throughputWithPeriod()) > 0,
               is.logical(throughput$clickTable))

    if(!throughput$clickTable){
      tableData <- copy(throughputWithPeriod())
    } else {
      tableData <- copy(throughput$tableData)
    }

    tableData <- tableData[, c("Date", addlTableColumns, throughputData$filterCols), with = FALSE]

    DT::datatable(copy(tableData),
                  options = list(lengthMenu = c(20, 50),
                                 scrollX    = TRUE,
                                 server     = TRUE))
  })
}
