#'@title load data from velocimeter into data.frame
#'
#'@details a \code{velocimeterK} function for loading sensor data from a file. Assumes the following format:
#'  1   Burst counter
#'  2   Ensemble counter                 (1-65536)
#'  3   Velocity (Beam1|X|East)          (m/s)
#'  4   Velocity (Beam2|Y|North)         (m/s)
#'  5   Velocity (Beam3|Z|Up)            (m/s)
#'  6   Amplitude (Beam1)                (counts)
#'  7   Amplitude (Beam2)                (counts)
#'  8   Amplitude (Beam3)                (counts)
#'  9   SNR (Beam1)                      (dB)
#'  10   SNR (Beam2)                      (dB)
#'  11   SNR (Beam3)                      (dB)
#'  12   Correlation (Beam1)              (%)
#'  13   Correlation (Beam2)              (%)
#'  14   Correlation (Beam3)              (%)
#'  15   Pressure                         (dbar)
#'  16   Analog input 1
#'  17   Analog input 2
#'  18   Checksum                         (1=failed)
#'
#'@param \code{file.nm} a valid file name.
#'@param \code{folder.nm} the folder where the data file resides.
#'@return a velocimeterK data.frame.
#'@keywords load.ADV
#'@examples 
#'folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.v <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'@export
#'
load.ADV <- function(file.nm,folder.nm='/Users/jread/Documents/R/velocimeterK/supporting data/'){
  
  
  file.loc <- file.path(folder.nm,file.nm)
  data.v <- read.table(file.loc)
  return(data.v)
}