\name{temp.calc}
\alias{temp.calc}
\title{block-averaged temperature}
\usage{
  temp.calc(data.sen, window.idx, freq = 32)
}
\arguments{
  \item{\code{data.sens}}{a data.frame created with
  load.ADV, with the window.idx column}

  \item{\code{window.idx}}{window.idx column from adv
  data.frame.}
}
\value{
  a vector of averaged values
}
\description{
  Water temperature average for within block \cr
}
\details{
  a \code{velocimeterK} function for averaging water
  temperature measurements from ADV sensor (1Hz).\cr
}
\examples{
folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
file.nm <- "ICACOS04.dat"
data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
data.sen <- load.sen(file.nm="ICACOS04.sen")

temp.calc(window.adv$window.idx)
}
\keyword{temp.calc}

