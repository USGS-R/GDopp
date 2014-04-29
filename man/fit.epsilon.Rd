\name{fit.epsilon}
\alias{fit.epsilon}
\title{fit epsilon from block of ADV data}
\usage{
  fit.epsilon(chunk.adv, freq = 32, lower = 0.5, upper = 8)
}
\arguments{
  \item{\code{chunk.adv}}{a data.frame created with
  load.ADV, with the window.idx column}

  \item{\code{freq}}{a numeric, in Hz, that represents the
  input data}

  \item{\code{lower}}{a numeric, in Hz, that represents the
  lower bounds for range of epsilon calculation}

  \item{\code{upper}}{a numeric, in Hz, that represents the
  upper bounds for range of epsilon calculation}
}
\value{
  a single value of epsilon estimating from the temporal
  block given.
}
\description{
  solves for epsilon from ADV data in the inertial subrange
  assuming Zappa et al. 2003 \cr
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
fit.epsilon(window.adv[window.adv$window.idx==7, ],freq=32)
}
\references{
  Zappa, Christopher J., Peter A. Raymond, Eugene A.
  Terray, and Wade R. McGillis. "Variation in surface
  turbulence and the gas transfer velocity over a tidal
  cycle in a macro-tidal estuary." Estuaries 26, no. 6
  (2003): 1401-1415.
}
\keyword{fit.epsilon}

