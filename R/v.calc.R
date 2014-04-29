#'@title gets along-stream mean velocity
#'@description 
#'gets along-stream mean velocity  \cr
#'
#'@details a \code{velocimeterK} function for calculating along-stream mean velocity.\cr 
#'
#'@param \code{chunk.adv} a data.frame created with load.ADV, with the window.idx column
#'@return a single mean along-stream velocity value
#'@keywords v.calc
#'@examples 
#'folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
#'chunk.adv <- window.adv[window.adv$window.idx==7, ]
#'v.calc(chunk.adv)
#'@export
v.calc <- function(chunk.adv){
  
  veloc.sqrd <- chunk.adv$velocity.Y^2 + chunk.adv$velocity.X^2
  veloc <- sqrt(veloc.sqrd)
  
  mn.veloc <- mean(veloc,na.rm=TRUE)
  
  return(mn.veloc)
  
}