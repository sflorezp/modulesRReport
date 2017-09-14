#' @import shiny
#' @title Widget Rango de Fecha Sys.Date
#' @description Esta es una funcion para obtener un widget de shiny que permite usar un combo selector de rango de fechas la fecha actual.
#' @param id Es el id del elemento a referenciar
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces
#' graficas usando Shiny. La funcion solamente recibe el parametro \code{id} del elemento para poder ser referencido en el script server.R,
#' Para poder ser referenciado en el script server..
#' @examples
#' dateTodayUI("date1")
#' dateTodayUI("date2")
#' @export
dateTodayUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::dateRangeInput(
    ns(id),
    label = "Rango entre fechas",
    start = Sys.Date(), end = Sys.Date()
  )
}

#' @import shiny
#' @import lubridate
#' @title Widget Rango de Fecha del Mes Anterior al Actual
#' @description Esta es una funcion para obtener un widget de shiny que permite usar un combo selector de rango de fechas para el mes anterior al actual.
#' @param id Es el id del elemento a referenciar
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces
#' graficas usando Shiny. La funcion solamente recibe el parametro \code{id} del elemento para poder ser referencido en el script server.R,
#' Para poder ser referenciado en el script server..
#' @examples
#' dateLastMonthUI("date1")
#' dateLastMonthUI("date2")
#' @export
dateLastMonthUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::dateRangeInput(
    ns(id),
    label = "Rango entre fechas",
    start = paste(substr(Sys.Date() - lubridate::day(Sys.Date()), 1, 7),"-01"), end = Sys.Date()- lubridate::day(Sys.Date())
  )
}

#' @import shiny
#' @import lubridate
#' @title Widget Rango de Fecha del Mes Anterior al Actual
#' @description Esta es una funcion para obtener un widget de shiny que permite usar un combo selector de rango de fechas para el anio anterior al actual.
#' @param id Es el id del elemento a referenciar
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces
#' graficas usando Shiny. La funcion solamente recibe el parametro \code{id} del elemento para poder ser referencido en el script server.R,
#' Para poder ser referenciado en el script server..
#' @examples
#' dateLastYearUI("date1")
#' dateLastYearUI("date2")
#' @export
dateLastYearUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::dateRangeInput(
    ns(id),
    label = "Rango entre fechas",
    start = paste(substr(Sys.Date()-365,1,4), "-01-01", sep = ""), end = paste(substr(Sys.Date()-365,1,4), "-12-31", sep = "")
  )
}

#' @import shiny
#' @title Widget para Botones
#' @description Esta es una funcion para obtener un widget de shiny que permite crear un boton en Shiny.
#' @param id Es el id del elemento a referenciar
#' @param nombre Es el nombre que tendra el boton
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces
#' graficas usando Shiny. La funcion recibe el parametro \code{id} del elemento para poder ser referenciado en el script server.R,
#' el parametro \code{nombre} se usara para definir el nombre que va a tener el boton
#' @examples
#' buttonUI("idGen","Generar")
#' @export
buttonUI <- function(id, nombre) {
  ns <- shiny::NS(id)
  shiny::tags$div(shiny::actionButton(ns(id), nombre, styleclass = "success"), align="center")
}

#' @import shiny
#' @title Widget Tipos de Agrupamientos comenzando desde Horario
#' @description Esta es una funcion para obtener un widget de shiny que permite usar un selector de agrupamiento para las opciones Horario, Diario, Mensual o Anual.
#' @param id Es el id del elemento a referenciar
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces
#' graficas usando Shiny. La funcion solamente recibe el parametro \code{id} del elemento para poder ser referenciado en el script server.R,
#' Para poder ser referenciado en el script server..
#' @examples
#' agrupaHUI("agrup")
#' @export
agrupaHUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::selectInput(
    inputId=ns(id),
    label=HTML("Agrupamiento"),
    choices = c("Horario", "Diario", "Mensual", "Anual"),
    selected = "Horario"
  )
}

#' @import shiny
#' @title Widget Tipos de Agrupamientos comenzando desde Diario
#' @description Esta es una funcion para obtener un widget de shiny que permite usar un selector de agrupamiento para las opciones Diario, Mensual o Anual.
#' @param id Es el id del elemento a referenciar
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces
#' graficas usando Shiny. La funcion solamente recibe el parametro \code{id} del elemento para poder ser referenciado en el script server.R,
#' Para poder ser referenciado en el script server..
#' @examples
#' agrupaDUI("agrup")
#' @export
agrupaDUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::selectInput(
    inputId=ns(id),
    label=HTML("Agrupamiento"),
    choices = c("Diario", "Mensual", "Anual"),
    selected = "Diario"
  )
}


#' @import shiny
#' @import dygraphs
#' @title Mostrar Salida Dygraphs Time Series
#' @description Esta es una funcion para mostrar objetos de graficas de Dygraphs.
#' @param id Es el id del elemento a referenciar
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces,
#' sirve para generar graficas de Serie de Tiempo usando la libreria dygraphs
#' @examples
#' timeSeriesDygraphsUI("plot1")
#' timeSeriesDygraphsUI("plot2")
#' @export
timeSeriesDygraphsUI <- function(id) {
  ns <- shiny::NS(id)
  dygraphs::dygraphOutput(ns("plot"))
}

#' @import dygraphs
#' @title Grafica T.S Dygraphs
#' @description Esta es una funcion que sirve para generar una grafica de series de tiempo en Dygraphs.
#' @param input Argumento Obligatorio
#' @param output Argumento Obligatorio
#' @param session Argumento Obligatorio
#' @param data Objeto del tipo xts con los datos a graficar
#' @param titulo Titulo de la grafica
#' @param ejey Titulo del eje y
#' @details Esta funcion hace parte del set de elementos a usar en el script server.R para facilitar la generacion de graficas usando Shiny.
#' La funcion tiene tres argumentos obligatorios \code{input}, \code{output}, \code{session}, estos siempre los usan los modulos en Shiny.
#' El argumento \code{data} tiene que ser del tipo xts, el argumento \code{titulo} corresponde con el titulo principal de la grafica y el argumento \code{ejey}
#' es para poner el titulo en el eje y de la grafica. Debido a que este es un modulo, su llamado es especial, ver los ejemplos de como se debe llamar esta funcion.
#' @examples
#' callModule(timeSeriesDygraphs, "plot1", trmDia_xts, "TRM diaria", "[COP/USD]")
#' callModule(timeSeriesDygraphs, "plot2", trmDia_xts, "TRM diaria", "[COP/USD]")
#' @export
timeSeriesDygraphs <- function(input, output, session, data, titulo, ejey) {
  output$plot <- dygraphs::renderDygraph({
    dygraphs::dygraph(data, main = titulo) %>%
      dygraphs::dyHighlight(highlightCircleSize = 5,
                            highlightSeriesBackgroundAlpha = 0.3,
                            hideOnMouseOut = FALSE) %>%
      dygraphs::dyRangeSelector() %>%
      dygraphs::dyAxis("y", ejey)
  })
}

#' @import shiny
#' @import plotly
#' @title Mostrar Salida Plotly Time Series
#' @description Esta es una funcion para mostrar objetos de graficas de Series de Tiempo usando Plotly
#' @param id Es el id del elemento a referenciar
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces,
#' sirve para generar una grafica de Serie de Tiempo usando Plotly
#' @examples
#' timeSeriesPlotlyUI("plot1")
#' timeSeriesPlotlyUI("plot2")
#' @export
timeSeriesPlotlyUI <- function(id) {
  ns <- shiny::NS(id)
  plotly::plotlyOutput(ns("plot"))
}

#' @import plotly
#' @title Grafica T.S Plotly
#' @description Esta es una funcion que sirve para generar una grafica de series de tiempo.
#' @param input Argumento Obligatorio
#' @param output Argumento Obligatorio
#' @param session Argumento Obligatorio
#' @param datosx Vector que Corresponde con las Fechas, debe estar en formato POSIXt
#' @param datosy Vector que Corresponde con los valores que van en el eje y
#' @param titulo Titulo de la grafica
#' @param ejey Titulo del eje y
#' @param suavizada Si su valor esta en True, debe mostrar una linea suavizada de los datos
#' sino mostrata los puntos con los valores reales
#' @details Esta funcion hace parte del set de elementos a usar en el script server.R para facilitar la generacion de graficas usando Shiny.
#' La funcion tiene tres argumentos obligatorios \code{input}, \code{output}, \code{session}, estos siempre los usan los modulos en Shiny.
#' El argumento \code{datosx} tiene que ser del tipo POSIXt, \code{datosy} corresponde con los valores del ejey, el argumento \code{titulo}
#' corresponde con el titulo principal de la grafica y el argumento \code{ejey} es para poner el titulo en el eje y de la grafica.
#' \code{suavizada} es un parametro que indicasi se mostraran los datos con una linea suavizada o se muestran los datos reales.
#' Debido a que este es un modulo, su llamado es especial, ver los ejemplos de como se debe llamar esta funcion.
#' @examples
#' callModule(timeSeriesPlotly, "plot1", trmDia_xts$x, trmDia_xts$y, "TRM diaria", "[COP/USD]", F)
#' callModule(timeSeriesPlotly, "plot2", trmDia_xts$x, trmDia_xts$y, "TRM diaria", "[COP/USD]", T)
#' @export
timeSeriesPlotly <- function(input, output, session, datosx, datosy, titulo, ejey, suavizada) {
  output$plot <- plotly::renderPlotly({
    if(suavizada == T) {
      p <- plotly::plot_ly(x = datosx, y = ~fitted(loess(datosy ~ as.numeric(datosx))), mode = "lines")
      p <- plotly::layout(p, title = titulo, yaxis = list(title = ejey), xaxis = list(rangeslider = list(type = "date")))
      p
    } else {
      p <- plotly::plot_ly(x = ~datosx, y = ~datosy, mode = "lines")
      p <- plotly::layout(p, title = titulo, yaxis = list(title = ejey), xaxis = list(title = "", rangeslider = list(type = "date")))
      p
    }
  })
}

#' @import shiny
#' @import DT
#' @title Mostrar Tabla de Datos
#' @description Esta es una funcion para mostrar tablas de datos en la interfaz grafica del reporte y adicionalmente permite su
#' descarga en formato xlsx o PDF
#' @param id Es el id del elemento a referenciar
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces,
#' sirve para generar una grafica de Serie de Tiempo usando el Packete DT
#' @examples
#' dataTableUI("plot1")
#' dataTableUI("plot2")
#' @export
dataTableUI <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("table"))
}

#' @import DT
#' @title Generar Tabla de Datos
#' @description Esta es una funcion que sirve para generar una tabla de datos.
#' @param input Argumento Obligatorio
#' @param output Argumento Obligatorio
#' @param session Argumento Obligatorio
#' @param data DataFrame con los datos que se quieren visualizar
#' @param nomFichero Nombre que tendra el fichero al ser descargado tanto en excel como PDF
#' @details Esta funcion hace parte del set de elementos a usar en el script server.R para facilitar la generacion de tablas de datos en Shiny.
#' La funcion tiene tres argumentos obligatorios \code{input}, \code{output}, \code{session}, estos siempre los usan los modulos en Shiny.
#' El argumento \code{data} corresponde al dataFrame o set de datos que se quieren visualizar en la interfaz grafica, en la medida de lo posible
#' se espera que sea un objeto del tipo DataFrame, \code{nomFichero} corresponde al nombre que el usuario desea que el fichero tenga al ser descargado.
#' @examples
#' callModule(dataTable, "table1", data1, "TestFichero1")
#' callModule(dataTable, "table2", data2, "TestFichero2")
#' @export
dataTable <- function(input, output, session, data, nomFichero) {
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
                     buttons = list(list(extend = 'excel', text= 'Excel', filename = nomFichero, exportOptions = list(columns = ':visible')),
                                    list(extend = 'pdf', text= 'PDF', filename = nomFichero, exportOptions = list(columns = ':visible'))
                     )
      )
    )
  }, server = F)
}

#' @import shiny
#' @title Widget Loading Bar
#' @description Esta es una funcion que se activa cuando se esta realizando algun calculo.
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces
#' graficas usando Shiny. La funcion detecta cuando el sistema esta ocupado realizando algun calculo y muestra en pantalla
#' unas barras azules que indican calculos, desaparecen cuando el sistema se libera
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
#' @title Agrupar Datos Por Algun Tipo y Sumar los Valores
#' @description Esta funcion sirve para agrupar datos de un dataFrame por algun tipo de agrupamiento y sumar los valores de la columna indicada en uno de sus parametros,
#' la flexibilidad de esta funcion radica en que permite agrupar por el numero de columnas que el usuario requiera#' @param data Set de Datos
#' @param agrupa Tipo de Agrupamiento
#' @param columnaFecha Numero de Columna donde esta la Fecha, comenzando desde el 1
#' @param columnaPeriodo Numero de Columna donde esta el Periodo, comenzando desde el 1, si los datos no tienen periodo poner valor de cero para no ser tenido en cuenta
#' @param columnaVal Numero de Columna donde esta el Valor que se desea Sumar, comenzando desde el 1
#' @param ... Se deben indicar las Columnas por las cuales se desea realizar el agrupamiento
#' @details Esta funcion sirve como ayuda a la hora de realizar calculos que requieran agrupar informacion y permite indicar
#' cual es la columna que se quiere sumar y cuales son las columndas por las cuales se quieren agrupar los datos
#' @examples
#' addGroupByType(trmDiaria, "Mensual", 1, 0, 2, "FECHA")
#' @export
addGroupByType <- function(data, agrupa, columnaFecha, columnaPeriodo, columnaVal, ...) {
  names(data)[columnaVal] <- "VALOR"
  if(agrupa == "Horario" && columnaPeriodo != 0) {
    data[,columnaFecha] <- paste(data[,columnaFecha], " ", data[,columnaPeriodo], ":00:00",sep="")
    data[,columnaFecha] <- as.POSIXct(strptime(data[,columnaFecha], "%Y-%m-%d %H:%M:%S"))
    data <- select_(data, .dots = data %>% select(...) %>%  colnames(), columnaVal)
  } else if(agrupa == "Diario") {
    data <- data %>%
      group_by_(.dots = data %>% select(...) %>%  colnames()) %>%
      summarise("VALOR" = sum(VALOR))
  } else if(agrupa == "Mensual") {
    data[,columnaFecha] <- substr(data[,columnaFecha], 1, 7)
    data[,columnaFecha] <-
      as.POSIXct(
        strptime(
          paste(data[,columnaFecha], "01", sep="-"),
          "%Y-%m-%d"
        )
      )
    data <- data %>%
      group_by_(.dots = data %>% select(...) %>%  colnames()) %>%
      summarise("VALOR" = sum(VALOR))
  } else if(agrupa == "Anual") {
    data[,columnaFecha] <- substr(data[,columnaFecha], 1, 4)
    data[,columnaFecha] <-
      as.POSIXct(
        strptime(
          paste(data[,columnaFecha],"01", "01",sep="-"),
          "%Y-%m-%d"
        )
      )
    data <- data %>%
      group_by_(.dots = data %>% select(...) %>%  colnames()) %>%
      summarise("VALOR" = sum(VALOR))
  }
}





