#' Standard for agility charts
#' @param input is passed from Shiny server
#' @param output is passed from Shiny server
#' @param session is passed from Shiny server
#' @keywords filters
#' @export
#' @examples
# agilityStandard <- function(input, output, session, agilityData, keyword,
agilityStandard <- function(input, output, session, agilityData, keyword,
                            boxTitle                     = "Agility",
                            agilityActionButtonGroupList = NULL,
                            actionButtonInputFunction    = NULL,
                            filterDefaultSelectedValues  = NULL,
                            showTable                    = FALSE,
                            addlTableColumns             = NULL){

  agilityWithActionButtonFilter <- reactive({
    if(!is.null(actionButtonInputFunction) & !is.null(agilityActionButtonGroupList)){
      data <- actionButtonInputFunction(data = copy(agilityData$data), inputValue = input[[agilityActionButtonGroupList$inputId]])
    } else {
      data <- copy(agilityData$data)
    }
    data
  })

  ###################################################################################### UI, Widgets

  filterCols <- reactive({
    filters <- agilityData$filterCols
    data.table(col       = filters,
               inputName = paste0(keyword, gsub(" ", "", filters)))
  })

  output[[paste0("set", keyword, "DateRange")]] <- renderUI({
    shiny::req(agilityWithActionButtonFilter())

    dates <- copy(agilityWithActionButtonFilter())$Date

    minDate   <- min(dates)
    maxDate   <- max(dates)

    # div(id = paste0(keyword, "DateRange", input$resetRedlineInput),
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
    shiny::req(agilityWithActionButtonFilter(), filterCols())
    filterWidgets <- mapply(function(csvColName, inputName){
      inputChoices <- as.character(sort(unique(agilityWithActionButtonFilter()[[csvColName]])))
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
                                            choices  = "None",
                                            selected = "None",
                                            multiple = FALSE)))

    div(id = paste0(keyword, input[[paste0("reset", keyword, "Input")]]),
        chieR::organizeWidgets(filters = filterWidgets, dateLensWidgets = fixedWidgets, 3))
  })

  output[[paste0(keyword, "UI")]] <- renderUI({
    shiny::req(is.logical(showTable))
    if(showTable){
      tableOutput <- reactableOutput(paste0(keyword, "Table"))
    } else {
      tableOutput <- NULL
    }
    if(!is.null(agilityActionButtonGroupList)){
      agilityActionButtonGroup <- radioGroupButtons(inputId  = agilityActionButtonGroupList$inputId,
                                                    label    = NULL,
                                                    choices  = agilityActionButtonGroupList$choices,
                                                    status   = "primary")
    } else {
      agilityActionButtonGroup <- NULL
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
         fluidRow(column(width = 12,
                         box(width       = NULL,
                             title       = boxTitle,
                             status      = "primary",
                             solidHeader = TRUE,
                             agilityActionButtonGroup,
                             plotlyOutput(paste0(keyword, "Plot"),
                                          height = "590px"),
                             tableOutput,
                             align = "center"))))
  })

  ###################################################################################### Calcs

  # Filter based on UI widgets
  agilityFiltered <- reactive({
    shiny::req(agilityWithActionButtonFilter(),
               input[[paste0(keyword, "DateRange")]])

    data  <- copy(agilityWithActionButtonFilter())

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
  observeEvent(input[[paste0("reset", keyword, "Input")]], {
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

    tableData <- tableData[, c("Date", addlTableColumns, agilityData$filterCols), with = FALSE]

    reactable(tableData,
              # defaultColDef   = colDef(vAlign      = "center",
              #                          header      = function(value){ gsub(".", " ", value, fixed = TRUE) },
              #                          cell        = function(value){ format(value, nsmall = 0) },
              #                          align       = "center",
              #                          html        = TRUE,
              #                          minWidth    = 20,
              #                          headerStyle = list(background = "#f7f7f8")),
              # columns         = list(`Title`   = colDef(width = 500),
              #                        `SLA Met` = colDef(
              #                          cell = function(value) {
              #                            if(value  == "No") shiny::icon("times-circle", class = "fas",
              #                                                           style = "color: #D83B01") else shiny::icon("check-circle", class = "fas",
              #                                                                                                      style = "color: #107C10")
              #                          }
              #                        )),
              striped         = TRUE,
              bordered        = TRUE,
              highlight       = TRUE)
    # DT::datatable(copy(tableData),
    #               options = list(lengthMenu = c(20, 50),
    #                              scrollX    = TRUE,
    #                              server     = TRUE))
  })
}
