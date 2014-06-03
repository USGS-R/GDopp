\name{load.sen}
\alias{load.sen}
\title{load sensor diagnostic data}
\usage{
  load.sen(file.nm,
    folder.nm = "/Users/jread/Documents/R/GDopp/supporting data/")
}
\arguments{
  \item{\code{file.nm}}{a valid file name.}

  \item{\code{folder.nm}}{the folder where the data file
  resides.}
}
\value{
  a GDopp data.frame.
}
\description{
  Assumes the following format: \cr \tabular{rlll}{ 1 \tab
  Month \tab (1-12) \cr 2 \tab Day \tab (1-31)\cr 3 \tab
  Year \tab \cr 4 \tab Hour \tab (0-23)\cr 5 \tab Minute
  \tab (0-59)\cr 6 \tab Second \tab (0-59) \cr 7 \tab Error
  code \tab \cr 8 \tab Status code \tab \cr 9 \tab Battery
  voltage \tab (V) \cr 10 \tab Speed of sound \tab (m/s)
  \cr 11 \tab Heading \tab (°) \cr 12 \tab Pitch \tab (°)
  \cr 13 \tab Roll \tab (°) \cr 14 \tab Temperature \tab
  (°C) \cr 15 \tab Analog input \tab \cr 16 \tab Checksum
  \tab (1=failed)\cr }
}
\details{
  a \code{GDopp} function for loading sensor diagnostic
  data from a file.\cr
}
\examples{
folder.nm <- '/Users/jread/Documents/R/GDopp/supporting data/'
file.nm <- "ICACOS04.sen"
data.sen <- load.sen(file.nm=file.nm, folder.nm =folder.nm)
}
\keyword{load.sen}

