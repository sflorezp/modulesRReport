#' @import shiny
#' @title SyS Date Range Widget
#' @description This function is to obtain an Shiny Widget to generates a Sys Date Selector on user interface.
#' @param id Element Identifier
#' @details This function belongs to script ui.R elements to easily creates user interfaces.
#' This function only have a parameter \code{id} to be referenced in server.R.
#' @examples
#' dateRangeTodayUI("date1")
#' dateRangeTodayUI("date2")
#' @export
dateRangeTodayUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::dateRangeInput(
    ns(id),
    label = "Rango entre fechas",
    start = Sys.Date(), end = Sys.Date()
  )
}

#' @import shiny
#' @import lubridate
#' @title Date Past Month Range Widget
#' @description This function is to obtain an Shiny Widget to generates a Past month Selector on user interface.
#' @param id Element Identifier
#' @details This function belongs to script ui.R elements to easily creates user interfaces.
#' This function only have a parameter \code{id} to be referenced in server.R.
#' @examples
#' dateRangeLastMonthUI("date1")
#' dateRangeLastMonthUI("date2")
#' @export
dateRangeLastMonthUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::dateRangeInput(
    ns(id),
    label = "Rango entre fechas",
    start = paste(substr(Sys.Date() - lubridate::day(Sys.Date()), 1, 7),"-01"), end = Sys.Date()- lubridate::day(Sys.Date())
  )
}

#' @import shiny
#' @import lubridate
#' @title Date Past Year Range Widget
#' @description This function is to obtain an Shiny Widget to generates a Past year Selector on user interface.
#' @param id Element Identifier
#' @details This function belongs to script ui.R elements to easily creates user interfaces.
#' This function only have a parameter \code{id} to be referenced in server.R.
#' @examples
#' dateRangeLastYearUI("date1")
#' dateRangeLastYearUI("date2")
#' @export
dateRangeLastYearUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::dateRangeInput(
    ns(id),
    label = "Rango entre fechas",
    start = paste(substr(Sys.Date()-365,1,4), "-01-01", sep = ""), end = paste(substr(Sys.Date()-365,1,4), "-12-31", sep = "")
  )
}

#' @import shiny
#' @title Date Only a Day Widget
#' @description This function is to obtain an Shiny Widget to generates a day Selector, Sys Date by default.
#' @param id Element Identifier
#' @details This function belongs to script ui.R elements to easily creates user interfaces.
#' This function only have a parameter \code{id} to be referenced in server.R.
#' @examples
#' dateTodayUI("date1")
#' dateTodayUI("date2")
#' @export
dateTodayUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::dateInput(ns(id), label = "Fecha", value = Sys.Date()-day(Sys.Date()))
}

#' @import shiny
#' @title Button Widget
#' @description This function is to obtain an Shiny Widget to generates a Button to makes any action on user interface.
#' @param id Element Identifier
#' @param name Button Name
#' @details This function belongs to script ui.R elements to easily creates user interfaces.
#' This funcion has two params, \code{id} to be referenced in server.R and
#' \code{name} to show in user interface the button name.
#' @examples
#' buttonUI("idGen","Generar")
#' @export
buttonUI <- function(id, name) {
  ns <- shiny::NS(id)
  shiny::tags$div(shiny::actionButton(ns(id), name, styleclass = "success"), align="center")
}

#' @import shiny
#' @title Grouping Types Widget
#' @description This function is to obtain an Shiny Widget to generates and grouping type selector with options Horario, Diario Mensual y Anual.
#' @param id Element Identifier
#' @param isHorary When the Value is TRUE begins in 'Horario' else 'Diario'
#' @details This function belongs to script ui.R elements to easily creates user interfaces.
#' This function has two parameters \code{id} to be referenced in server.R and isHorary to
#' show a selector begins in Horary if it is TRUE else Diario.
#' @examples
#' agrupaUI("agrup", T)
#' agrupaUI("agrup", F)
#' @export
agrupaUI <- function(id, isHorary) {
  ns <- shiny::NS(id)
  if(isHorary) {
    shiny::selectInput(
      inputId=ns(id),
      label=HTML("Agrupamiento"),
      choices = c("Horario", "Diario", "Mensual", "Anual"),
      selected = "Horario"
    )
  } else {
    shiny::selectInput(
      inputId=ns(id),
      label=HTML("Agrupamiento"),
      choices = c("Diario", "Mensual", "Anual"),
      selected = "Diario"
    )
  }
}

#' @import shiny
#' @import dygraphs
#' @title Dygraphs Objects
#' @description This function allows generate Dygraphs Objects e.g time series Charts.
#' @param id Element Identifier
#' @details This function belongs to ui.R to easily creates user interfaces.
#' It allows showing many types of different charts that can be generated using Dygraphs Package.
#' @examples
#' dygraphObjectUI("plot1")
#' dygraphObjectUI("plot2")
#' @export
dygraphObjectUI <- function(id) {
  ns <- shiny::NS(id)
  dygraphs::dygraphOutput(ns("plot"))
}

#' @import dygraphs
#' @title Dygraphs Time Series Chart
#' @description This function is to generates a time series chart using Dygraphs
#' @param input Required Param by Shiny Modules
#' @param output Required Param by Shiny Modues
#' @param session Required Param by Shiny Modules
#' @param data Data with xts format
#' @param title Chart Title
#' @param yaxis y axis Title
#' @details This function belongs to server.R script to easily generates a time series chart using Dygraphs.
#' This function has three required params \code{input}, \code{output}, \code{session} that always is used by Shiny Modules.
#' The \code{data} param must be in xts format, \code{title} param means to the main title of chart and the \code{yaxis}
#' param is to set the y axis title. How it used Shiny Modules, The function will be called using the callModule function,
#' the first param is the function to be called, the second param is the identifier used in dygraphObjectUI("id") and the other
#' params are the same after the session param. See the examples please.
#' @examples
#' callModule(timeSeriesDygraphs, "plot1", trmDia_xts, "TRM diaria", "[COP/USD]")
#' callModule(timeSeriesDygraphs, "plot2", trmDia_xts, "TRM diaria", "[COP/USD]")
#' @export
timeSeriesDygraphs <- function(input, output, session, data, title, yaxis) {
  output$plot <- dygraphs::renderDygraph({
    dygraphs::dygraph(data, main = title) %>%
      dygraphs::dyHighlight(highlightCircleSize = 5,
                            highlightSeriesBackgroundAlpha = 0.3,
                            hideOnMouseOut = FALSE) %>%
      dygraphs::dyRangeSelector() %>%
      dygraphs::dyAxis("y", yaxis)
  })
}

#' @import shiny
#' @import plotly
#' @title Plotly Objects
#' @description This function is to generates a time series chart using Dygraphs
#' @param id Element Identifier
#' @details This function belongs to ui.R to easily creates user interfaces.
#' It allows showing many types of different charts that can be generated using Plotly Package.
#' @examples
#' plotlyObjectUI("plot1")
#' plotlyObjectUI("plot2")
#' @export
plotlyObjectUI <- function(id) {
  ns <- shiny::NS(id)
  plotly::plotlyOutput(ns("plot"))
}

#' @import plotly
#' @title Plotly Time Series Chart
#' @description This function is used to generates time series charts using Plotly Package
#' @param input Required Param
#' @param output Required Param
#' @param session Required Param
#' @param xdata Array with Dates in POSIXt Format
#' @param ydata Array with Values
#' @param title Main Title
#' @param yaxis y Axis Title
#' @param isSmooth When is True must be showed an Smooth Line else the Real Value
#' @details This function belongs to server.R script to easily generates a time series chart using Dygraphs.
#' This function has three required params \code{input}, \code{output}, \code{session} that always is used by Shiny Modules.
#' The \code{xdata} param must be in POSIXt format, \code{ydata} param means to y axis values, \code{title} param
#' means to maint title, \code{yaxis} param is to set y axis title, \code{isSmooth} param say when must be show and Smooth Line
#' or Real Values. How it used Shiny Modules, The function will be called using the callModule function, in
#' the first param is the function to be called, the second param is the identifier used in plotlyObjectUI("id") and the other
#' params are the same after the session param. See the examples please.
#' @examples
#' callModule(timeSeriesPlotly, "plot1", trmDia_xts$x, trmDia_xts$y, "TRM diaria", "[COP/USD]", F)
#' callModule(timeSeriesPlotly, "plot2", trmDia_xts$x, trmDia_xts$y, "TRM diaria", "[COP/USD]", T)
#' @export
timeSeriesPlotly <- function(input, output, session, xdata, ydata, title, yaxis, isSmooth) {
  output$plot <- plotly::renderPlotly({
    if(isSmooth) {
      p <- plotly::plot_ly(x = xdata, y = ~fitted(loess(ydata ~ as.numeric(xdata))), mode = "lines")
      p <- plotly::layout(p, title = title, yaxis = list(title = yaxis), xaxis = list(title = "", rangeslider = list(type = "date")))
      p
    } else {
      p <- plotly::plot_ly(x = ~xdata, y = ~ydata, mode = "lines")
      p <- plotly::layout(p, title = title, yaxis = list(title = yaxis), xaxis = list(title = "", rangeslider = list(type = "date")))
      p
    }
  })
}

#' @import plotly
#' @title Plotly Bar Chart
#' @description This function is used to generates a bar chart using Plotly Package
#' @param input Required Param
#' @param output Required Param
#' @param session Required Param
#' @param xdata Array with numeric values, usually is a percentage
#' @param ydata Array with character values, usually is a name
#' @param xaxis x Axis Title
#' @param setElement Element name to be highlighted
#' @details This function belongs to server.R script to easily generates a bar chart using Dygraphs.
#' This function has three required params \code{input}, \code{output}, \code{session} that always is used by Shiny Modules.
#' The \code{ydata} param means to names in y axis, \code{xdata} param is realted to x axis values, usually it must a percentage,
#' the \code{xaxis} param is to set x axis title, the \code{setElement} param is used to set a element name to be highlighted in the chart.
#' How it used Shiny Modules, The function will be called using the callModule function, in
#' the first param is the function to be called, the second param is the identifier used in plotlyObjectUI("id") and the other
#' params are the same after the session param. See the examples please.
#' @examples
#' callModule(barPlotly, "plot1", data1$x, data2$y, "Participacion %", "ISGG")
#' callModule(barPlotly, "plot2", data1$x, data2$y, "Ingresos %")
#' @export
barPlotly <- function(input, output, session, xdata, ydata, xaxis, setElement = NULL) {

}

#' @import shiny
#' @import DT
#' @title Data Table Widget
#' @description This function is used to generates a Data Table With PDF and xlsx Buttons to Download using DT Package
#' @param id Element Identifier
#' @details This function belongs to script ui.R elements to easily creates user interfaces.
#' This funcion has only one param \code{id} to be referenced in server.R
#' @examples
#' dataTableUI("plot1")
#' dataTableUI("plot2")
#' @export
dataTableUI <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("table"))
}

#' @import DT
#' @title Generates Data Table
#' @description This function is used to generates a Data Table
#' @param input Required Argument
#' @param output Required Argument
#' @param session Required Argument
#' @param data DataFrame to ve visualized
#' @param fileName File Name when the Data Table is downloaded
#' @details This function belongs to server.R script to easily generates a bar chart using Dygraphs.
#' This function has three required params \code{input}, \code{output}, \code{session} that always is used by Shiny Modules.
#' The \code{data} param is the data frame to ve visualized as a Data Table in the user interface, \code{fileName} param is the name of the file
#' when the user click the downlaod button.
#' How it used Shiny Modules, The function will be called using the callModule function, in
#' the first param is the function to be called, the second param is the identifier used in dataTableUI("id") and the other
#' params are the same after the session param. See the examples please.
#' @examples
#' callModule(dataTable, "table1", data1, "TestFichero1")
#' callModule(dataTable, "table2", data2, "TestFichero2")
#' @export
dataTable <- function(input, output, session, data, fileName) {
  output$table <- DT::renderDataTable({
    DT::datatable(
      data,
      filter = 'top',
      rownames = FALSE,
      extensions = c('Buttons'),
      options = list(dom = 'Bfrtip',
                     autoWidth = TRUE,
                     language = list(url = '//genmdx94/reports/Spanish.json'),
                     columnDefs = list(list(className = 'dt-left', targets="_all")),
                     fixedHeader = T,
                     pageLength = 15,
                     lengthChange = FALSE,
                     buttons = list(list(extend = 'excel', text= 'Excel', filename = fileName, exportOptions = list(columns = ':visible')),
                                    list(extend = 'pdf', text= 'PDF', filename = fileName, exportOptions = list(columns = ':visible'))
                     )
      )
    )
  }, server = F)
}

#' @import shiny
#' @title Loading Bar Widget
#' @description This function is used to generates a bussy indicator in the user interface when the app is doing any calculation
#' @details This function belongs to script ui.R elements to easily creates user interfaces.
#' It detects when the System is bussy doing any calculation and shows a busy indicator in the user screen, it stop when the
#' calculation is finished
#' @examples
#' loadBarUI()
#' @export
loadBarUI <- function() {
  shiny::conditionalPanel(condition="$('html').hasClass('shiny-busy')",
    shiny::tags$hr(),
    shiny::div(id = "plot-container",
      shiny::div(class = "plotlybars-wrapper",
        shiny::div(class="plotlybars",
          shiny::div(class="plotlybars-bar b1"),
          shiny::div(class="plotlybars-bar b2"),
          shiny::div(class="plotlybars-bar b3"),
          shiny::div(class="plotlybars-bar b4"),
          shiny::div(class="plotlybars-bar b5"),
          shiny::div(class="plotlybars-bar b6"),
          shiny::div(class="plotlybars-bar b7")
        ),
        shiny::div(class="plotlybars-text", p("Calculando"))
      )
    ),
    shiny::tags$br(),
    shiny::tags$br(),
    shiny::tags$br()
  )
}

#' @import dplyr
#' @title Group Data By Time Agrupation and Sum Any Column Value
#' @description This function belongs to script server.R and is used to group Data Frames for any Time Agrupation and Sum Any Column Values,
#' This function is very useful because is possible group by one or more columns.
#' @param data Data Frame
#' @param groupType Time Agrupation
#' @param dateCol Date Column Number were is the Date of the Data Frame
#' @param periodCol Period Column Number were is the Period of the Data Frame in case that it be with 'Horario' Time Agrupation
#' @param valueCol Value Column Number were is the Value of the Data Frame to be summarized
#' @param ... After valueCol Param can be referenciated one or more column numbers to be grouped
#' @details This function belongs to server.R script to easily realize operations to group data by any time agrupation
#' @examples
#' addGroupByType(trmDiaria, "Mensual", 1, 0, 2, "FECHA")
#' @export
addGroupByType <- function(data, groupType, dateCol, periodCol, valueCol, ...) {
  names(data)[valueCol] <- "VALOR"
  if(agrupa == "Horario" && periodCol != 0) {
    data[, dateCol] <- paste(data[, dateCol], " ", data[, periodCol], ":00:00",sep="")
    data[, dateCol] <- as.POSIXct(strptime(data[, dateCol], "%Y-%m-%d %H:%M:%S"))
    data <- select_(data, .dots = data %>% select(...) %>%  colnames(), valueCol)
  } else if(agrupa == "Diario") {
    data <- data %>%
      group_by_(.dots = data %>% select(...) %>%  colnames()) %>%
      summarise("VALOR" = sum(VALOR))
  } else if(agrupa == "Mensual") {
    data[, dateCol] <- substr(data[, dateCol], 1, 7)
    data[, dateCol] <-
      as.POSIXct(
        strptime(
          paste(data[, dateCol], "01", sep="-"),
          "%Y-%m-%d"
        )
      )
    data <- data %>%
      group_by_(.dots = data %>% select(...) %>%  colnames()) %>%
      summarise("VALOR" = sum(VALOR))
  } else if(agrupa == "Anual") {
    data[, dateCol] <- substr(data[, dateCol], 1, 4)
    data[, dateCol] <-
      as.POSIXct(
        strptime(
          paste(data[, dateCol],"01", "01",sep="-"),
          "%Y-%m-%d"
        )
      )
    data <- data %>%
      group_by_(.dots = data %>% select(...) %>%  colnames()) %>%
      summarise("VALOR" = sum(VALOR))
  }
}





