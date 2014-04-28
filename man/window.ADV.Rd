\name{window.ADV}
\alias{window.ADV}
\title{window ADV data}
\usage{
  window.ADV(data.adv, freq = 32, window.mins = 10)
}
\arguments{
  \item{\code{data.adv}}{a data.frame created with
  load.ADV}

  \item{\code{freq}}{a numeric, in Hz, that represents the
  input data}

  \item{\code{window.mins}}{a numeric of number of minutes
  desired for outputs}
}
\value{
  a velocimeterK data.frame with a column for window.idx.
}
\description{
  creates index values for ADV data according to temporal
  windowing \cr
}
\details{
  a \code{velocimeterK} function for splitting sensor data
  into temporal segments.\cr
}
\examples{
folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
file.nm <- "ICACOS04.dat"
data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
}
\keyword{window.ADV}

