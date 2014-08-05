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
#'@keywords methods, math
#'@examples 
#'\dontrun{
#'folder.nm <- system.file('extdata', package = 'GDopp') 
#'file.nm <- "ALQ102.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window_ADV(data.adv,freq=32,window.mins=10)
#'data.sen <- load.sen(file.nm="ALQ102.sen", folder.nm = folder.nm)
#'
#'window_coord(data.sen,window.adv$window.idx,freq=32)
#'}
#'@export

window_coord <- function(data.sen,window.idx,freq=32){
  
  win_heading <- match_time(value=data.sen$heading, window.idx, freq)$value
  win_pitch <- match_time(value=data.sen$pitch, window.idx, freq)$value
  win_roll <- match_time(value=data.sen$roll, window.idx, freq)$value
  
  df <- data.frame('heading'=win_heading, 'pitch' = win_pitch, 'roll' = win_roll)
  
  return(df)
}