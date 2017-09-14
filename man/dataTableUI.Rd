% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myModules.R
\name{dataTableUI}
\alias{dataTableUI}
\title{Mostrar Tabla de Datos}
\usage{
dataTableUI(id)
}
\arguments{
\item{id}{Es el id del elemento a referenciar}
}
\description{
Esta es una funcion para mostrar tablas de datos en la interfaz grafica del reporte y adicionalmente permite su
descarga en formato xlsx o PDF
}
\details{
Esta funcion hace parte del set de elementos a usar en el script ui.R para facilitar la creacion de interfaces,
sirve para generar una grafica de Serie de Tiempo usando el Packete DT
}
\examples{
dataTableUI("plot1")
dataTableUI("plot2")
}