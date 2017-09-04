#' @import shiny
#' @title Widget Rango de Fecha Sys.Date
#' @description Esta es una funcion para obtener un widget de shiny que permite usar un combo selector de rango de fechas.
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
#' @import dygraphs
#' @title Mostrar Salida Dygraph Chart
#' @description Esta es una funcion para mostrar objetos de graficas de Dygraphs.
#' @param id Es el id del elemento a referenciar
#' @details Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces,
#' sirve para mostrar graficas de la libreria dygraphs
#' @examples
#' chartDygraphsUI("plot1")
#' chartDygraphsUI("plot2")
#' @export
chartDygraphsUI <- function(id) {
  ns <- shiny::NS(id)
  dygraphs::dygraphOutput(ns("plot"))
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
#' callModule(chartDygraphs, "plot1", trmDia_xts, "TRM diaria", "[COP/USD]")
#' callModule(chartDygraphs, "plot2", trmDia_xts, "TRM diaria", "[COP/USD]")
#' @export
chartDygraphs <- function(input, output, session, data, titulo, ejey) {
  output$plot <- dygraphs::renderDygraph({
    dygraphs::dygraph(data, main = titulo) %>%
      dygraphs::dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = FALSE) %>%
      dygraphs::dyRangeSelector() %>%
      dygraphs::dyAxis("y", ejey)
  })
}
