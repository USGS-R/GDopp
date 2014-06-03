#'@title window ADV data
#'@description 
#'creates index values for ADV data according to temporal windowing \cr
#'
#'@details a \code{GDopp} function for splitting sensor data into temporal segments.\cr 

#'@param \code{data.adv} a data.frame created with load.ADV
#'@param \code{freq} a numeric, in Hz, that represents the input data
#'@param \code{window.mins} a numeric of number of minutes desired for outputs
#'@return a GDopp data.frame with a column for window.idx.
#'@keywords window.ADV
#'@examples 
#'folder.nm <- '/Users/jread/Documents/R/GDopp/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
#'@export
#'
window.ADV <- function(data.adv,freq=32,window.mins=10){
  
  secs2mins <- 60 # number of seconds in a minute
  num.samples <- nrow(data.adv)
  length.mins <- num.samples/freq/secs2mins
  block.idx <- floor(seq(1,length.mins/window.mins+1,length.out=num.samples))
  window.adv <- cbind(data.adv,data.frame("window.idx"=block.idx))
  
  return(window.adv)
}