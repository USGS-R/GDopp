% Generated by roxygen2 (4.0.1): do not edit by hand
\name{window_adv}
\alias{window_adv}
\title{window ADV data}
\usage{
window_adv(data.adv, freq = 32, window.mins = 10)
}
\arguments{
\item{data.adv}{a data.frame created with load.ADV}

\item{freq}{a numeric, in Hz, that represents the input data}

\item{window.mins}{a numeric of number of minutes desired for outputs}
}
\value{
a GDopp data.frame with a column for window.idx.
}
\description{
creates index values for ADV data according to temporal windowing \cr
}
\details{
a \code{GDopp} function for splitting sensor data into temporal segments.\cr
}
\examples{
\dontrun{
folder.nm <- system.file('extdata', package = 'GDopp')
file.nm <- "ALQ102.dat"
data.adv <- load_adv(file.nm=file.nm, folder.nm =folder.nm)
window.adv <- window_adv(data.adv,freq=32,window.mins=10)
}
}
\keyword{methods}

