#'@title checks adv
#'@description 
#'checks data for various quality metrics  \cr
#'
#'@details a \code{velocimeterK} function for checking data quality.\cr 
#'
#'@param \code{chunk.adv} a data.frame created with load.ADV, with the window.idx column
#'@param \code{tests} a character array of test names
#'@return failed, T or F
#'@keywords check.adv
#'@references
#'Vachon, Dominic, Yves T. Prairie, and Jonathan J. Cole. 
#'"The relationship between near-surface turbulence and gas transfer 
#'velocity in freshwater systems and its implications for floating chamber measurements of gas exchange." 
#'Limnology and Oceanography 55, no. 4 (2010): 1723.
#'
#'@examples 
#'folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
#'chunk.adv <- window.adv[window.adv$window.idx==7, ]
#'check.adv(chunk.adv)
#'@export

check.adv <- function(chunk.adv,tests=NULL){
  
  
  #signal-to-noise ratio was greater than 15 db
  failed <- signal.noise.check(chunk.adv)
  return(failed)
  
}

signal.noise.check <- function(chunk.adv){
  threshold <- 15
  s2n.rat.X <- mean(chunk.adv$signal.rat.X,na.rm=TRUE)
  s2n.rat.Y <- mean(chunk.adv$signal.rat.Y,na.rm=TRUE)
  s2n.rat.Z <- mean(chunk.adv$signal.rat.Z,na.rm=TRUE)
  failed = FALSE
  if (any(c(s2n.rat.X,s2n.rat.Y,s2n.rat.Z)>threshold)){
    failed = TRUE
  }
  return(failed)
}