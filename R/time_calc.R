#'@title block-averaged time
#'@description 
#'time average for within block  \cr
#'
#'@details a \code{GDopp} function for averaging time observations from ADV sensor (1Hz).\cr 
#'assumes one second spin-up for high-frequency measurement, and a likely incomplete final second for high frequency.
#'
#'@param data.sen a data.frame created with load.ADV, with the window.idx column
#'@param window.idx window.idx column from adv data.frame.
#'@param freq the frequency (Hz) of the measurements
#'@return a vector of block mean time
#'@keywords methods, math
#'@examples 
#'\dontrun{
#'folder.nm <- system.file('extdata', package = 'GDopp') 
#'file.nm <- "ALQ102.dat"
#'data.adv <- load_adv(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window_adv(data.adv,freq=32,window.mins=10)
#'data.sen <- load_sen(file.nm="ALQ102.sen", folder.nm = folder.nm)
#'
#'time_calc(data.sen,window.adv$window.idx,freq=32)
#'}
#'@export
time_calc <- function(data.sen,window.idx,freq=32,calc.time=FALSE){
  
  pass_vars <- data.sen[, names(data.sen) %in% c("month","day","year","hour","minute","second")]
  
  time_vecs <- match_time(value=pass_vars, window.idx, freq)
  
  time <- get_block_time(time_vecs)
  return(time_vecs)
}

# test this for date crossover!
get_block_time <- function(time_vecs){

  time_floor <- floor(time_vecs)
  dates <- ISOdatetime(time_floor[, 3],time_floor[, 1],time_floor[, 2],time_floor[, 4],time_floor[, 5],time_floor[, 6])
  return(dates)
}