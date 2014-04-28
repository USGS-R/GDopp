#'@title window ADV data
#'@description 
#'creates index values for ADV data according to temporal windowing \cr
#'
#'@details a \code{velocimeterK} function for splitting sensor data into temporal segments.\cr 

#'@param \code{data.adv} a data.frame created with load.ADV
#'@return a velocimeterK data.frame with a column for window.idx.
#'@keywords window.ADV
#'@examples 
#'folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window.ADV(data.adv)
#'@export
#'
window.ADV <- function(data.adv){
  
}