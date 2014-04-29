#'@title convert epsilon to k600
#'@description 
#'Uses Zappa et al. 2007 to convert epsilon to k600.  \cr
#'
#'@details a \code{velocimeterK} function for converting epsilon to k600.\cr 
#'
#'@param \code{epsilon} Turbulence dissipation rate
#'@param \code{temperature} Temperature in °C of near surface water
#'@param \code{nu} Constant of proportionality
#'@return a single k600 value
#'@keywords epsilon2k
#'@examples 
#'folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
#'chunk.adv <- window.adv[window.adv$window.idx==7, ]
#'epsilon <- fit.epsilon(chunk.adv,freq=32)
#'epsilon2k(epsilon,temperature=20,nu=0.2)
#'@export

epsilon2k <- function(epsilon,temperature=20,nu=0.2){
  
  
  return(k600)
}