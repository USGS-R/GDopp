#'@title checks adv
#'@description 
#'checks data for various quality metrics  \cr
#'
#'@details a \code{GDopp} function for checking data quality.\cr 
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
#'Kitaigorodskii, S. A., M. A. Donelan, J. L. Lumley, and E. A. Terray. 
#'"Wave-turbulence interactions in the upper ocean. Part II. Statistical 
#'characteristics of wave and turbulent components of the random velocity 
#'field in the marine surface layer." Journal of Physical Oceanography 13, no. 11 (1983): 1988-1999.
#'
#'@examples 
#'folder.nm <- '/Users/jread/Documents/R/GDopp/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
#'chunk.adv <- window.adv[window.adv$window.idx==7, ]
#'check.adv(chunk.adv)
#'@export

check.adv <- function(chunk.adv,tests=NULL){
  
  
  #signal-to-noise ratio was greater than 15 db
  
  #failed	<-	do.call(match.fun(method),list(data.in=data.in,reject.criteria=reject.criteria))
  failed <- signal.noise.check(chunk.adv)
  return(failed)
  
}

signal.noise.check <- function(chunk.adv){
  threshold <- 15
  s2n.rat.X <- mean(chunk.adv$signal.rat.X,na.rm=TRUE)
  s2n.rat.Y <- mean(chunk.adv$signal.rat.Y,na.rm=TRUE)
  s2n.rat.Z <- mean(chunk.adv$signal.rat.Z,na.rm=TRUE)
  failed = FALSE
  if (any(c(s2n.rat.X,s2n.rat.Y,s2n.rat.Z) < threshold)){
    failed = TRUE
  }
  return(failed)
}

beam.correlation <- function(chunk.adv){
  # Lien & D'Asaro 2006
  #Bursts were discarded if the average correlation of any of three ADV beams was lower than 0.9
}

frozen.turb.check <- function(chunk.adv){
  failed = FALSE
  V <- v.calc(chunk.adv)
  v. <- chunk.adv$velocity.Z
  mn.v. <- mean(v.) # mean of fluctuating velocity
  nrm.v. <- v.-mn.v.
  r.v. <- sqrt(sum(nrm.v.^2)/length(nrm.v.))
  if ((r.v./V)^3 >= 1){
    failed = TRUE
  }
  return(failed)
}