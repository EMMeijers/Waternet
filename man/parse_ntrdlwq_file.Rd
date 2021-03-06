% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers.R
\name{parse_ntrdlwq_file}
\alias{parse_ntrdlwq_file}
\title{read DELWAQ ntrdlwq.poi file into an R dataframe object,
the file contains the relation between the Netter flow objects and the DELWAQ segments.}
\usage{
parse_ntrdlwq_file(filename)
}
\arguments{
\item{filename}{ntrdlwq file to be read.}
}
\value{
R dataframe containing the folowing columns: 1) segment,2) reach .
}
\description{
read DELWAQ ntrdlwq.poi file into an R dataframe object,
the file contains the relation between the Netter flow objects and the DELWAQ segments.
}
