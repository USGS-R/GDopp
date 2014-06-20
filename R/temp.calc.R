#'@title block-averaged temperature
#'@description 
#'Water temperature average for within block  \cr
#'
#'@details a \code{GDopp} function for averaging water temperature measurements from ADV sensor (1Hz).\cr 
#'assumes one second spin-up for high-frequency measurement, and a likely incomplete final second for high frequency.
#'
#'@param data.sens a data.frame created with load.ADV, with the window.idx column
#'@param window.idx window.idx column from adv data.frame.
#'@return a vector of averaged values
#'@keywords temp.calc
#'@examples 
#'\dontrun{
#'folder.nm <- '../../../Desktop/Science Projects/GDopp/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
#'data.sen <- load.sen(file.nm="ICACOS04.sen")
#'
#'temp.calc(data.sen,window.adv$window.idx,freq=32)
#'}
#'@export

temp.calc <- function(data.sen,window.idx,freq=32,calc.time=FALSE){
  
  if (!is.null(dim(window.idx))){stop('window.idx must be a 1D vector')}
  
  num.diag <- nrow(data.sen)
  num.hf <- length(window.idx)
  
  expect.hf <- num.diag*freq
  hf.dif <- expect.hf-num.hf 
  
  step.drop <- ceiling(hf.dif/freq)
  if (hf.dif<0){stop('actual high-frequency measurements should not exceed frequency*diagnostic file. Check files')}
  
  if (step.drop>=1){
    data.sen <- data.sen[-1, ]
  }
  
  
  t.win <- window.idx[seq(1,length(window.idx),freq)]
  pad.num <- nrow(data.sen) - length(t.win)
  rep.pad <- rep(x=tail(t.win,1),pad.num)
  t.win <- c(t.win,rep.pad)
  un.blocks <- unique(t.win)
  
  length.out <- length(un.blocks)
  temps <- data.sen$temperature

  if (length(temps) != length(t.win)){stop('win blocks are different lengths than temperature array')}
  
  block.temp <- vector(length=length.out)
  time <- rep(as.POSIXct('1900-01-01'),length.out)
  for (i in seq_len(length(un.blocks))){
    block.temp[i] <- mean(temps[t.win==un.blocks[i]])
    if (calc.time){
      time[i] <- get.sen.time(chunk.sen=data.sen[t.win==un.blocks[i], ])
    }
  }

  if (calc.time){
    return(data.frame("time"=time,"temperature"=block.temp))
  } else {
    return(block.temp)
  }
}

get.sen.time <- function(chunk.sen){
  d.vals <- chunk.sen[, 1:6]
  d.tail <- as.numeric(tail(d.vals,1))
  d.head <- as.numeric(head(d.vals,1))
  date.1 <- ISOdatetime(d.head[3],d.head[1],d.head[2],d.head[4],d.head[5],d.head[6])
  date.2 <- ISOdatetime(d.tail[3],d.tail[1],d.tail[2],d.tail[4],d.tail[5],d.tail[6])
  
  mn.date <- mean(c(date.1,date.2))
  return(mn.date)
}