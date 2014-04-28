#'@title load ADV data
#'@description 
#'Assumes the following format: \cr
#'\tabular{rlll}{
#'1 \tab Burst counter   \tab  \cr
#'2 \tab Ensemble counter  \tab (1-65536)\cr
#'3 \tab Velocity (Beam1|X|East)  \tab (m/s)\cr
#'4 \tab Velocity (Beam2|Y|North) \tab (m/s)\cr
#'5 \tab Velocity (Beam3|Z|Up)    \tab (m/s)\cr
#'6 \tab Amplitude (Beam1)  \tab (counts) \cr
#'7 \tab Amplitude (Beam2)  \tab (counts) \cr
#'8 \tab Amplitude (Beam3)  \tab (counts) \cr
#'9 \tab SNR (Beam1)  \tab (dB) \cr
#'10 \tab SNR (Beam2)  \tab (dB) \cr
#'11 \tab SNR (Beam3)  \tab (dB) \cr
#'12 \tab Correlation (Beam1)  \tab (percent) \cr
#'13 \tab Correlation (Beam2)  \tab (percent) \cr
#'14 \tab Correlation (Beam3)  \tab (percent) \cr
#'15 \tab Pressure \tab (dbar) \cr
#'16 \tab Analog input 1 \cr
#'17 \tab Analog input 2 \cr
#'18 \tab Checksum \tab (1=failed)\cr
#'}
#'
#'@details a \code{velocimeterK} function for loading sensor data from a file.\cr 

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
  
  drop.cols <- c('checksum')
  adv.dat.names <- c('burst.num','ensemble.num','velocity.X','velocity.Y','velocity.Z',
                     'amplitude.X','amplitude.Y','amplitude.Z',
                     'signal.rat.X','signal.rat.Y','signal.rat.Y',
                     'correlation.X','correlation.Y','correlation.Z',
                     'pressure','analog.1','analog.2','checksum')
  file.loc <- file.path(folder.nm,file.nm)
  data.adv <- read.table(file.loc)
  
  names(data.adv) <- adv.dat.names
  
  rmv.i <- data.adv$checksum==1
  if (any(rmv.i)){
    warning('some checksum failures. Removing errant values')
    data.adv <- data.adv[!rmv.i, ]
  }
  
  data.adv <- data.adv[,!(names(data.adv) %in% drop.cols)]
  
  return(data.adv)
}