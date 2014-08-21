#'@title block-averaged temperature
#'@description 
#'Water temperature average for within block  \cr
#'
#'@details a \code{GDopp} function for averaging water temperature measurements from ADV sensor (1Hz).\cr 
#'assumes one second spin-up for high-frequency measurement, and a likely incomplete final second for high frequency.
#'
#'@param data.sen a data.frame created with load.ADV, with the window.idx column
#'@param window.idx window.idx column from adv data.frame.
#'@param freq the frequency (Hz) of the measurements
#'@param calc.time a boolean for including a time vector in the output (will then return a data.frame)
#'@return a vector of averaged values
#'@keywords methods, math
#'@examples 
#'\dontrun{
#'folder.nm <- system.file('extdata', package = 'GDopp') 
#'file.nm <- "ALQ102.dat"
#'data.adv <- load_adv(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window_adv(data.adv,freq=32,window.mins=10)
#'data.sen <- load_sen(file.nm="ALQ102.sen", folder.nm = folder.nm)
#'
#'temp_calc(data.sen,window.adv$window.idx,freq=32)
#'}
#'@export

temp_calc <- function(data.sen,window.idx,freq=32,calc.time=FALSE){
  
  
  temps <- data.sen$temperature

  
  
  df <- match_time(value=temps, window.idx, freq)

  if (calc.time){
    names(df) <- c('time','temperature')
    return(df)
  } else {
    return(df$value)
  }
}
