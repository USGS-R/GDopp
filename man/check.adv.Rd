\name{check.adv}
\alias{check.adv}
\title{checks adv}
\usage{
  check.adv(chunk.adv, tests = NULL)
}
\arguments{
  \item{\code{chunk.adv}}{a data.frame created with
  load.ADV, with the window.idx column}

  \item{\code{tests}}{a character array of test names}
}
\value{
  failed, T or F
}
\description{
  checks data for various quality metrics \cr
}
\details{
  a \code{velocimeterK} function for checking data
  quality.\cr
}
\examples{
folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
file.nm <- "ICACOS04.dat"
data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
chunk.adv <- window.adv[window.adv$window.idx==7, ]
check.adv(chunk.adv)
}
\references{
  Vachon, Dominic, Yves T. Prairie, and Jonathan J. Cole.
  "The relationship between near-surface turbulence and gas
  transfer velocity in freshwater systems and its
  implications for floating chamber measurements of gas
  exchange." Limnology and Oceanography 55, no. 4 (2010):
  1723.

  Kitaigorodskii, S. A., M. A. Donelan, J. L. Lumley, and
  E. A. Terray. "Wave-turbulence interactions in the upper
  ocean. Part II. Statistical characteristics of wave and
  turbulent components of the random velocity field in the
  marine surface layer." Journal of Physical Oceanography
  13, no. 11 (1983): 1988-1999.
}
\keyword{check.adv}

