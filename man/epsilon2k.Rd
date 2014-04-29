\name{epsilon2k}
\alias{epsilon2k}
\title{convert epsilon to k600}
\usage{
  epsilon2k(epsilon, temperature = 20, nu = 0.2)
}
\arguments{
  \item{\code{epsilon}}{Turbulence dissipation rate}

  \item{\code{temperature}}{Temperature in °C of near
  surface water}

  \item{\code{nu}}{Constant of proportionality}
}
\value{
  a single k600 value
}
\description{
  Uses Zappa et al. 2007 to convert epsilon to k600.  \cr
}
\details{
  a \code{velocimeterK} function for converting epsilon to
  k600.\cr
}
\examples{
folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
file.nm <- "ICACOS04.dat"
data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
chunk.adv <- window.adv[window.adv$window.idx==7, ]
epsilon <- fit.epsilon(chunk.adv,freq=32)
epsilon2k(epsilon,temperature=20,nu=0.2)
}
\keyword{epsilon2k}

