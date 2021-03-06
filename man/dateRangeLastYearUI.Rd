% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myModules.R
\name{dateRangeLastYearUI}
\alias{dateRangeLastYearUI}
\title{Date Past Year Range Widget}
\usage{
dateRangeLastYearUI(id)
}
\arguments{
\item{id}{Element Identifier}
}
\description{
This function is to obtain an Shiny Widget to generates a Past year Selector on user interface.
}
\details{
This function belongs to script ui.R elements to easily creates user interfaces.
This function only have a parameter \code{id} to be referenced in server.R.
}
\examples{
dateRangeLastYearUI("date1")
dateRangeLastYearUI("date2")
}
