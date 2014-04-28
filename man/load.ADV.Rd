\name{load.ADV}
\alias{load.ADV}
\title{load ADV data}
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
  Assumes the following format: \cr \tabular{rlll}{ 1 \tab
  Burst counter \tab \cr 2 \tab Ensemble counter \tab
  (1-65536)\cr 3 \tab Velocity (Beam1|X|East) \tab (m/s)\cr
  4 \tab Velocity (Beam2|Y|North) \tab (m/s)\cr 5 \tab
  Velocity (Beam3|Z|Up) \tab (m/s)\cr 6 \tab Amplitude
  (Beam1) \tab (counts) \cr 7 \tab Amplitude (Beam2) \tab
  (counts) \cr 8 \tab Amplitude (Beam3) \tab (counts) \cr 9
  \tab SNR (Beam1) \tab (dB) \cr 10 \tab SNR (Beam2) \tab
  (dB) \cr 11 \tab SNR (Beam3) \tab (dB) \cr 12 \tab
  Correlation (Beam1) \tab (percent) \cr 13 \tab
  Correlation (Beam2) \tab (percent) \cr 14 \tab
  Correlation (Beam3) \tab (percent) \cr 15 \tab Pressure
  \tab (dbar) \cr 16 \tab Analog input 1 \cr 17 \tab Analog
  input 2 \cr 18 \tab Checksum \tab (1=failed)\cr }
}
\details{
  a \code{velocimeterK} function for loading sensor data
  from a file.\cr
}
\examples{
folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
file.nm <- "ICACOS04.dat"
data.v <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
}
\keyword{load.ADV}

