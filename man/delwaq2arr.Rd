\name{his2arr}
\alias{his2arr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{convert Delwaq <.his> or <.map> file into an R array object}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(DelwaqR)
arr <- his2arr(filename = "DATA/NZBLOOM.his", timestamp = F, begintime = "2003-01-01 00:00:00")
dimnames(arr)
submod <- c("Chlfa", "OXY")
locmod <- c("NZR6NW020", "NZR9TS010")
df <- arr2df(arr, locmod=locmod, submod=submod)
df$value[df$variable == "fResptot"] <- -df$value[df$variable == "fResptot"]
library(ggplot2)
plot <- ggplot(df, aes(time, value))
plot +
  geom_line(aes(color = variable), size = 1) +
  geom_point(aes(color = variable), fill = "white",  shape = 21, size = 4) +
  facet_grid((. ~ location))
}