\name{load.ADV}
\alias{load.ADV}
\title{load data from velocimeter into data.frame}
\usage{
  load.ADV(file.nm,
    folder.nm = "/Users/jread/Documents/R/velocimeterK/supporting data/")
}
\arguments{
  \item{\code{file.nm}}{a valid file name.}

  \item{\code{folder.nm}}{the folder where the data file
  resides.}
}
\value{
  a velocimeterK data.frame.
}
\description{
  load data from velocimeter into data.frame
}
\details{
  a \code{velocimeterK} function for loading sensor data
  from a file.
}
\examples{
folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
file.nm <- "ICACOS04.dat"
data.v <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
}
\keyword{load.ADV}

