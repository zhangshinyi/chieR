#' Standard for trend charts
#' @param input is passed from Shiny server
#' @param output is passed from Shiny server
#' @param session is passed from Shiny server
#' @keywords filters
#' @export
#' @examples
trendStandard <- function(input, output, session, trendData, keyword, title,
                          percentTF                   = FALSE,
                          timeIntervalChoices         = c("Day", "Week", "Month"),
                          defaultTimeInterval         = "Month",
                          addlTableColumns            = NULL,
                          showTable                   = FALSE,
                          averageByPeriod             = FALSE,
                          normalizeBynRow             = FALSE,
                          filterDefaultSelectedValues = NULL,
                          defaultLens                 = "None"){
  trend <- reactiveValues()

  filterCols <- reactive({
    shiny::req(trendData$filterCols)
    filters <- trendData$filterCols
    data.table(col       = filters,
               inputName = paste0(keyword, gsub(" ", "", filters)))
  })

  output[[paste0("set", keyword, "DateRange")]] <- renderUI({
    shiny::req(trendData$data, trendData$semesterDates)

    minDate <- min(trendData$data$Date)
    maxDate <- max(trendData$semesterDates$Date)

    semesterStartDate <- min(trendData$semesterDates$Date)

    div(id = paste0(keyword, "DateRange", input[[paste0("reset", keyword, "Input")]]),
        dateRangeInput(paste0(keyword, "DateRange"),
                       label = "Date Range",
                       min   = minDate,
                       start = max(c(minDate, seq(semesterStartDate, length = 2, by = "-3 months")[2])),
                       end   = maxDate,
                       max   = maxDate,
                       width = "100%")
    )
  })

  output[[paste0("resetable", keyword, "Input")]] <- renderUI({
    shiny::req(trendData$data, filterCols())

    filterWidgets <- mapply(function(csvColName, inputName){
      inputChoices <- as.character(sort(unique(trendData$data[[csvColName]])))

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
                                            choices  = timeIntervalChoices,
                                            selected = defaultTimeInterval,
                                            multiple = FALSE)),
                         column(width = 2,
                                uiOutput(paste0("set", keyword, "DateRange"))),
                         column(width = 2,
                                pickerInput(paste0(keyword, "Lens"),
                                            label    = "Lens",
                                            choices  = c("None", trendData$filterCols),
                                            selected = defaultLens,
                                            multiple = FALSE)))

    div(id = paste0("redline", input[[paste0("reset", keyword, "Input")]]),
        chieR::organizeWidgets(filters = filterWidgets, dateLensWidgets = fixedWidgets, 3))
  })

  trendFiltered <- reactive({
    shiny::req(trendData$data)

    data  <- copy(trendData$data)

    for(i in 1:nrow(filterCols())){
      # print(i)
      shiny::req(input[[filterCols()[i]$inputName]])
      data <- data[get(filterCols()[i]$col) %in% input[[filterCols()[i]$inputName]]]
      # print(dim(data))
    }

    data
  })

  trendPeriodMapping <- reactive({
    shiny::req(input[[paste0(keyword, "TimeInterval")]],
               trendData$dateMapping)

    periodCol   <- paste0("formatted", input[[paste0(keyword, "TimeInterval")]])
    dateMapping <- copy(trendData$dateMapping)[, c("Date", periodCol), with = FALSE]
    setnames(dateMapping, periodCol, "Period")
    dateMapping
  })

  trendDatePeriod <- reactive({
    shiny::req(trendFiltered(), nrow(trendFiltered()) > 0,
               trendPeriodMapping())
    merge(copy(trendFiltered()),
          trendPeriodMapping(),
          by    = "Date",
          all.x = TRUE)
  })

  trendLensAggregated <- reactive({
    shiny::req(trendDatePeriod(), nrow(trendDatePeriod()) > 0,
               input[[paste0(keyword, "Lens")]],
               input[[paste0(keyword, "DateRange")]])

    dateRange <- input[[paste0(keyword, "DateRange")]]

    data <- copy(trendDatePeriod())[(Date >= min(dateRange)) & (Date <= max(dateRange))]

    if(averageByPeriod){
      periodMapping <- copy(trendPeriodMapping())[(Date >= min(dateRange)) & (Date <= max(dateRange))][, numDays := 1]
      numDays       <- periodMapping[, .(numDays = sum(numDays)), by = .(Period)]
      data          <- merge(data,
                             numDays,
                             by = "Period",
                             all.x = TRUE)[, Count := (Count / numDays)][, numDays := NULL]
    }

    if(input[[paste0(keyword, "Lens")]] != "None"){
      rowCount <- data[, .(nRows = length(Count)), by = Period]
      data     <- data[, .(Count = sum(Count)), by = c("Period", input[[paste0(keyword, "Lens")]])]
      data     <- merge(data, rowCount, by = "Period")
    } else {
      data <- data[, .(Count = sum(Count), nRows = length(Count)), by = Period][, None := "None"]
    }

    setnames(data, input[[paste0(keyword, "Lens")]], "lens")
    data
  })

  trendDateFilter <- reactive({
    shiny::req(trendLensAggregated(), nrow(trendLensAggregated()) > 0,
               input[[paste0(keyword, "DateRange")]])
    # Filter for valid periods
    dateMapping  <- copy(trendPeriodMapping())[(Date >= min(input[[paste0(keyword, "DateRange")]])) &
                                                 (Date <= max(input[[paste0(keyword, "DateRange")]]))]
    data         <- copy(trendLensAggregated())[Period %in% dateMapping$Period]

    # Fill in 0s for missing period/lens combinations
    periodLensCross <- data.table(tidyr::expand_grid(Period = unique(dateMapping$Period),
                                                     lens   = unique(data$lens)))
    data <- merge(periodLensCross, data, by = c("Period", "lens"), all.x = TRUE)
    data[is.na(Count), Count := 0]
    data[is.na(nRows), nRows := 0]
    setkey(data, Period, lens)

    if(normalizeBynRow){
      data[, Count := fifelse(nRows == 0, 0, 100 * Count / nRows)]
    }

    data[, nRows := NULL]

    return(data)
  })

  output[[paste0(keyword, "UI")]] <- renderUI({
    shiny::req(is.logical(showTable))
    if(showTable){
      tableOutput <- DT::dataTableOutput(paste0(keyword, "Table"))
    } else {
      tableOutput <- NULL
    }
    list(column(width = 12,
                div(style = "display:inline-block; float:right",
                    tipify(actionButton(paste0("reset", keyword, "Input"),
                                        "",
                                        icon = icon("redo")),
                           "Refresh settings",
                           placement = "top"))),
         uiOutput(paste0("resetable", keyword, "Input")),
         fluidRow(
           column(width = 12,
                  box(width       = NULL,
                      title       = title,
                      status      = "primary",
                      solidHeader = TRUE,
                      plotlyOutput(paste0(keyword, "Plot"),
                                   height = "590px"),
                      tableOutput,
                      align = "center")
           )))
  })

  output[[paste0(keyword, "Plot")]] <- renderPlotly({
    shiny::req(trendDateFilter())
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Creating trend plot...", value = 1)

    data <- melt(copy(trendDateFilter()), c("Period", "lens"))

    if(percentTF){
      data[, value := (value / 100)]
      hoverFormat <- ".2%"
      tickFormat  <- ".0%"
    } else {
      hoverFormat <- NULL
      tickFormat  <- NULL
    }

    chieR::plotlyChartWrapper(data            = data,
                              mode            = "bar",
                              barmode         = "relative",
                              showBarTotals   = TRUE,
                              percent         = percentTF,
                              hoverFormat     = hoverFormat,
                              tickFormat      = tickFormat,
                              barTotalDecimal = 0,
                              totalsBySign    = TRUE,
                              sourceName      = paste0(keyword, "Plot"))
  })

  if(showTable){
    observeEvent(event_data("plotly_click", source = paste0(keyword, "Plot")), {
      shiny::req(trendDatePeriod())
      clickInfo       <- event_data("plotly_click", source = paste0(keyword, "Plot"))
      lens            <- input[[paste0(keyword, "Lens")]]
      selectedLensVal <- as.character(levels(trendDatePeriod()[[lens]])[clickInfo$curveNumber + 1])
      data            <- copy(trendDatePeriod())[Period == clickInfo$x]
      if(lens %in% names(data)){
        data[, eval(lens) := as.character(get(lens))]
        if(selectedLensVal %in% data[[lens]]){
          data <- data[get(lens) == selectedLensVal]
        }
      }
      data[, Period := NULL]
      trend$tableData <- copy(data)
    })

    observeEvent(input[[paste0("reset", keyword, "Input")]], {
      trend$tableData <- copy(trendDatePeriod())
    })

    output[[paste0(keyword, "Table")]] <- DT::renderDataTable({
      shiny::req(trendDatePeriod())

      if(is.null(event_data("plotly_click", source = paste0(keyword, "Plot")))){
        data <- copy(trendDatePeriod())
      } else {
        data <- copy(trend$tableData)
      }

      shiny::req(nrow(data) > 0)

      data <- data[, c("Date", addlTableColumns, trendData$filterCols), with = FALSE]

      # Server or client side processing:
      # The server argument determines whether the data is processed on the server
      # side or the client (browser) side. If server = TRUE (the default), the
      # browser receives only the displayed data. If server = FALSE the browser
      # receives all the data, which can slow it down if the dataset is large.
      DT::datatable(copy(data),
                    options = list(lengthMenu = c(20, 50),
                                   scrollX    = TRUE,
                                   server     = TRUE))
    })
  }
}
