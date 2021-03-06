% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myModules.R
\name{addGroupByType}
\alias{addGroupByType}
\title{Group Data By Time Agrupation and Sum Any Column Value}
\usage{
addGroupByType(data, groupType, dateCol, periodCol, valueCol, ...)
}
\arguments{
\item{data}{Data Frame}

\item{groupType}{Time Agrupation}

\item{dateCol}{Date Column Number were is the Date of the Data Frame}

\item{periodCol}{Period Column Number were is the Period of the Data Frame in case that it be with 'Horario' Time Agrupation}

\item{valueCol}{Value Column Number were is the Value of the Data Frame to be summarized}

\item{...}{After valueCol Param can be referenciated one or more column numbers to be grouped}
}
\description{
This function belongs to script server.R and is used to group Data Frames for any Time Agrupation and Sum Any Column Values,
This function is very useful because is possible group by one or more columns.
}
\details{
This function belongs to server.R script to easily realize operations to group data by any time agrupation
}
\examples{
addGroupByType(trmDiaria, "Mensual", 1, 0, 2, "FECHA")
}
