#'@title block-averaged temperature
#'@description 
#'Water temperature average for within block  \cr
#'
#'@details a \code{velocimeterK} function for averaging water temperature measurements from ADV sensor (1Hz).\cr 
#'assumes one second spin-up for high-frequency measurement, and a likely incomplete final second for high frequency.
#'
#'@param \code{data.sens} a data.frame created with load.ADV, with the window.idx column
#'@param \code{window.idx} window.idx column from adv data.frame.
#'@return a vector of averaged values
#'@keywords temp.calc
#'@examples 
#'folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
#'data.sen <- load.sen(file.nm="ICACOS04.sen")
#'
#'temp.calc(data.sen,window.adv$window.idx,freq=32)
#'@export

temp.calc <- function(data.sen,window.idx,freq=32){
  
  if (!is.null(dim(window.idx))){stop('window.idx must be a 1D vector')}
  
  num.diag <- nrow(data.sen)
  num.hf <- length(window.idx)
  
  expect.hf <- num.diag*freq
  hf.dif <- expect.hf-num.hf 
  
  step.drop <- ceiling(hf.dif/freq)
  if (hf.dif<0){stop('actual high-frequency measurements should not exceed frequency*diagnostic file. Check files')}
  
  if (step.drop==2 | step.drop==1){
    #full first timepoint drop, partial drop on last, or no drop on last. Either are handled by seq(1,n,freq)
    data.sen <- data.sen[-1, ]
  } else if (step.drop>2){stop('missing multiple timepoints for high-frequency data. Check files')}
  
  # do nothing for no drops
  
  t.win <- window.idx[seq(1,length(window.idx),freq)]
  un.blocks <- unique(t.win)
  
  temps <- data.sen$temperature
  
  block.temp <- vector(length=length(un.blocks))
  for (i in seq_len(length(un.blocks))){
    block.temp[i] <- mean(temps[t.win==un.blocks[i]])
  }
  
  
  return(block.temp)
  
}