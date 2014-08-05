#'@title block-averaged coordinate shift
#'@description 
#'Coord system components averaged within a block  \cr
#'
#'@details a \code{GDopp} function for averaging coordinate measurements from ADV sensor (1Hz).\cr 
#'assumes one second spin-up for high-frequency measurement, and a likely incomplete final second for high frequency.
#'
#'@param data.sens a data.frame created with load.ADV, with the window.idx column
#'@param window.idx window.idx column from adv data.frame.
#'@param freq the frequency (Hz) of the measurements
#'@param calc.time a boolean for including a time vector in the output (will then return a data.frame)
#'@keywords methods, math
#'@examples 
#'\dontrun{
#'folder.nm <- system.file('extdata', package = 'GDopp') 
#'file.nm <- "ALQ102.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window_ADV(data.adv,freq=32,window.mins=10)
#'data.sen <- load.sen(file.nm="ALQ102.sen")
#'
#'window_coord(data.sen,window.adv$window.idx,freq=32)
#'}
#'@export

window_coord <- function(data.sen,window.idx,freq=32,calc.time=FALSE){
  
  
  temps <- data.sen$temperature
  
  df <- match_time(value=temps, window.idx, freq)
  
  if (calc.time){
    names(df) <- c('time','temperature')
    return(df)
  } else {
    return(df$value)
  }
}