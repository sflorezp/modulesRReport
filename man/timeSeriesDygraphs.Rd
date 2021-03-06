% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myModules.R
\name{timeSeriesDygraphs}
\alias{timeSeriesDygraphs}
\title{Dygraphs Time Series Chart}
\usage{
timeSeriesDygraphs(input, output, session, data, title, yaxis)
}
\arguments{
\item{input}{Required Param by Shiny Modules}

\item{output}{Required Param by Shiny Modues}

\item{session}{Required Param by Shiny Modules}

\item{data}{Data with xts format}

\item{title}{Chart Title}

\item{yaxis}{y axis Title}
}
\description{
This function is to generates a time series chart using Dygraphs
}
\details{
This function belongs to server.R script to easily generates a time series chart using Dygraphs.
This function has three required params \code{input}, \code{output}, \code{session} that always is used by Shiny Modules.
The \code{data} param must be in xts format, \code{title} param means to the main title of chart and the \code{yaxis}
param is to set the y axis title. How it used Shiny Modules, The function will be called using the callModule function,
the first param is the function to be called, the second param is the identifier used in dygraphObjectUI("id") and the other
params are the same after the session param. See the examples please.
}
\examples{
callModule(timeSeriesDygraphs, "plot1", trmDia_xts, "TRM diaria", "[COP/USD]")
callModule(timeSeriesDygraphs, "plot2", trmDia_xts, "TRM diaria", "[COP/USD]")
}
