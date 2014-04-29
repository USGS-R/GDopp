\name{v.calc}
\alias{v.calc}
\title{gets along-stream mean velocity}
\usage{
  v.calc(chunk.adv)
}
\arguments{
  \item{\code{chunk.adv}}{a data.frame created with
  load.ADV, with the window.idx column}
}
\value{
  a single mean along-stream velocity value
}
\description{
  gets along-stream mean velocity \cr
}
\details{
  a \code{velocimeterK} function for calculating
  along-stream mean velocity.\cr
}
\examples{
folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
file.nm <- "ICACOS04.dat"
data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
chunk.adv <- window.adv[window.adv$window.idx==7, ]
v.calc(chunk.adv)
}
\keyword{v.calc}

