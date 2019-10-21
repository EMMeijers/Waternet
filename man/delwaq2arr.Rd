\name{delwaq2arr}
\alias{delwaq2arr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{convert Delwaq <.his> or <.map> file into a R array object}
\description{
This function reads a delwaq binairy result file with extention <.map> or <.his> and put it into a R array object. 
}
\usage{
delwaq2arr(filename, timestamp = T, begintime = "1900-01-01 00:00:00")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
the <.his> or <.map> file to be converted.
}
}

\value{
An R array object of the Delwaq .his file named  \code{filename}. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Erwin Meijers,
}
\note{
script based on his2arr from Willem Stolte
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(Waternet)
arr <- delwaq2arr(filename = "DATA/testdata.his")
dimnames(arr)
submod <- c("OXY", "Cl")
locmod <- c("LOX003", "LOX005", "LOX007", "LOX009")
df <- arr2df(arr, locmod=locmod, submod=submod)
library(ggplot2)
plot <- ggplot(df, aes(time, value))
plot +
  geom_line(aes(color = variable), size = 1) +
  geom_point(aes(color = variable), fill = "white",  shape = 21, size = 4) +
  facet_grid((. ~ location))
}
