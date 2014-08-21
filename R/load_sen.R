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


#takes a data.frame (value) and samples it according to the window.idx
#returns a data.frame with the same names, that will match the window chunks
match_time <- function(value, window.idx, freq=32){
  if (!is.null(dim(window.idx))){stop('window.idx must be a 1D vector')}
  one_d <- F
  if (is.null(dim(value))){
    one_d <- T
  }
  
  is_date <- is_date_vec(value)
  
  df_names <- names(value)
  num_values <- ifelse(one_d,1,ncol(value))
  
  num_rows <- ifelse(one_d,length(value),nrow(value))
    
  num.hf <- length(window.idx)
  
  expect.hf <- num_rows*freq
  hf.dif <- expect.hf-num.hf 
  
  step.drop <- ceiling(hf.dif/freq)
  if (hf.dif<0){stop('actual high-frequency measurements should not exceed frequency*diagnostic file. Check files')}
  
  if (step.drop>=1){
    if (one_d){
      value <- value[-1]
    } else
      value <- value[-1, ]
  }
   
  t.win <- window.idx[seq(1,length(window.idx),freq)] # subsampling the window info
  
  #if (length(value) != length(t.win)){stop('win blocks are different lengths than measurement array')}
  
  pad.num <- num_rows - length(t.win) - 1
  rep.pad <- rep(x = tail(t.win, 1), pad.num)
  t.win <- c(t.win, rep.pad) # pad out to length with last val
  un.blocks <- unique(t.win)
  
  length.out <- length(un.blocks)
  
  block.value <- matrix(nrow = length.out, ncol = num_values) 
  
  for (j in seq_len(length.out)){
    u_i <- t.win==un.blocks[j]
    block.value[j, ] <- mean_cols(value, u_i, one_d, is_date) # passing a lot of unecessary data everytime here?
  }
  
  match_vals <- data.frame(block.value)
  names(match_vals) <- df_names
  
  if (is.null(df_names) & one_d){
    match_vals <- match_vals[, 1]
  }
  return(match_vals)
}

mean_cols <- function(vals, u_i, one_d, is_date){
  
  if (is_date){
    mid_i <- floor(median(which(u_i)))
    val_out <- as.numeric(vals[mid_i, ])
    return(val_out)
  }
  
  
  if (one_d){
    val_out <- mean(vals[u_i])
  } else {
    val_out <- colMeans(vals[u_i, ])
  }
  return(val_out)
}

is_date_vec <- function(value){
  if (is.null(names(value))){
    return(FALSE)
  } else if (all(names(value) %in% c("month","day","year","hour","minute","second"))){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

