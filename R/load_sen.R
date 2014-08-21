#'@title load sensor diagnostic data
#'@description 
#'Assumes the following format: \cr
#'\tabular{rlll}{
#'1 \tab Month   \tab (1-12) \cr
#'2 \tab Day  \tab (1-31)\cr
#'3 \tab Year \tab \cr
#'4 \tab Hour \tab (0-23)\cr
#'5 \tab Minute    \tab (0-59)\cr
#'6 \tab Second  \tab (0-59) \cr
#'7 \tab Error code  \tab  \cr
#'8 \tab Status code  \tab  \cr
#'9 \tab Battery voltage  \tab (V) \cr
#'10 \tab Speed of sound  \tab (m/s) \cr
#'11 \tab Heading  \tab (deg) \cr
#'12 \tab Pitch  \tab (deg) \cr
#'13 \tab Roll  \tab (deg) \cr
#'14 \tab Temperature  \tab (degC) \cr
#'15 \tab Analog input \tab  \cr
#'16 \tab Checksum \tab (1=failed)\cr
#'}
#'
#'@details a \code{GDopp} function for loading sensor diagnostic data from a file.\cr 

#'@param file.nm a valid file name.
#'@param folder.nm the folder where the data file resides.
#'@return a GDopp data.frame.
#'@keywords load.sen
#'@examples 
#'folder.nm <- system.file('extdata', package = 'GDopp') 
#'file.nm <- "ALQ102.sen"
#'data.sen <- load_sen(file.nm = file.nm, folder.nm = folder.nm)
#'@export
#'
load_sen <- function(file.nm, folder.nm){
  adv.sen.names <- c('month','day','year','hour','minute','second',
                     'error.cd','status.cd','batt.v','sound.speed',
                     'heading','pitch','roll','temperature','analog.in','checksum')
  
  data.sen <- read.table(file.path(folder.nm,file.nm))
  
  names(data.sen) <- adv.sen.names
  
  return(data.sen)
}


match_time <- function(value, window.idx, freq=32){
  if (!is.null(dim(window.idx))){stop('window.idx must be a 1D vector')}
  
  1d = F
  if (is.null(dim(value))){
    1d = T
  }
  
  num_values <- ifelse(1d, 1, ncol(value))
  
  num_rows <- ifelse(1d, length(value), nrow(value))
    
  num.hf <- length(window.idx)
  
  expect.hf <- num_rows*freq
  hf.dif <- expect.hf-num.hf 
  
  step.drop <- ceiling(hf.dif/freq)
  if (hf.dif<0){stop('actual high-frequency measurements should not exceed frequency*diagnostic file. Check files')}
  
  if (step.drop>=1){
    value <- ifelse(1d, value[-1], value[-1, ])
  }
  
  
  t.win <- window.idx[seq(1,length(window.idx),freq)]
  
  #if (length(value) != length(t.win)){stop('win blocks are different lengths than measurement array')}
  
  pad.num <- num_rows - length(t.win)
  rep.pad <- rep(x=tail(t.win,1),pad.num)
  t.win <- c(t.win,rep.pad)
  un.blocks <- unique(t.win)
  
  length.out <- length(un.blocks)
  
  block.value <- vector(length = length.out)

  for (i in seq_len(length(un.blocks))){
    block.value[i] <- mean(value[t.win==un.blocks[i]])
    chunk <- ifelse(1d, value[t.win==un.blocks[i]], value[t.win==un.blocks[i], ])
  }
  
  return(block.value)
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