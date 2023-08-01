#' Standard for trend charts
#' @param input is passed from Shiny server
#' @param output is passed from Shiny server
#' @param session is passed from Shiny server
#' @keywords filters
#' @export
#' @examples
trendStandard <- function(input, output, session, trendData, keyword, title, percentTF = FALSE, defaultTimeInterval = "Month"){
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
                                            choices  = c("Day", "Week", "Month"),
                                            selected = defaultTimeInterval,
                                            multiple = FALSE)),
                         column(width = 2,
                                uiOutput(paste0("set", keyword, "DateRange"))),
                         column(width = 2,
                                pickerInput(paste0(keyword, "Lens"),
                                            label    = "Lens",
                                            choices  = c("None", trendData$filterCols),
                                            selected = "None",
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
    data <- copy(trendDatePeriod())[(Date >= min(input[[paste0(keyword, "DateRange")]])) &
                                      (Date <= max(input[[paste0(keyword, "DateRange")]]))]

    if(input[[paste0(keyword, "Lens")]] != "None"){
      data <- data[, .(Count = sum(Count)), by = c("Period", input[[paste0(keyword, "Lens")]])]
    } else {
      data <- data[, .(Count = sum(Count)), by = c("Period")][, None := "None"]
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
    setkey(data, Period, lens)

    return(data)
  })

  output[[paste0(keyword, "UI")]] <- renderUI({
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
                              totalsBySign    = TRUE)
  })
}
