% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IO.R
\name{sobek2arr}
\alias{sobek2arr}
\title{convert SOBEK <.map> or <.his> file into an R array object,}
\usage{
sobek2arr(filename, timestamp = T, begintime = "1900-01-01 00:00:00")
}
\arguments{
\item{filename}{the <.his> or <.map> file to be converted.}
}
\value{
An R array object of the SOBEK <.his> or <.map> file named  \code{filename}.
}
\description{
convert SOBEK <.map> or <.his> file into an R array object,
}
\examples{
library(Waternet)
arr <- sobek2arr(filename = "data/testdata.his")
submod <- c("OXY", "Cl")
locmod <- c("LOX003","LOX009")
df <- arr2df(arr, locmod=locmod, submod=submod)
library(ggplot2)
plot <- ggplot(df, aes(time, value)) +
  geom_line(aes(color = variable), size = 1) +
  facet_grid((variable ~ location), scales = "free")
plot
}
