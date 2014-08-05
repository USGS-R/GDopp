#'@title fit epsilon from block of ADV data
#'@description 
#'solves for epsilon from ADV data in the inertial subrange assuming Zappa et al. 2003  \cr
#'
#'@details a \code{GDopp} function for splitting sensor data into temporal segments.\cr 
#'
#'@references
#'Zappa, Christopher J., Peter A. Raymond, Eugene A. Terray, and Wade R. McGillis. 
#'"Variation in surface turbulence and the gas transfer velocity over a tidal cycle in a macro-tidal estuary." 
#'Estuaries 26, no. 6 (2003): 1401-1415.
#'
#'@param chunk.adv a data.frame created with load.ADV, with the window.idx column
#'@param freq a numeric, in Hz, that represents the input data
#'@param lower a numeric, in Hz, that represents the lower bounds for range of epsilon calculation
#'@param upper a numeric, in Hz, that represents the upper bounds for range of epsilon calculation
#'@param diagnostic a boolean for diagnostic plot
#'@return a single value of epsilon estimating from the temporal block given. 
#'@keywords methods, math
#'@import oce
#'@examples 
#'\dontrun{
#'folder.nm <- system.file('extdata', package = 'GDopp') 
#'file.nm <- "ALQ102.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window_ADV(data.adv,freq=32,window.mins=10)
#'fit.epsilon(window.adv[window.adv$window.idx==7, ],freq=32)
#'}
#'@export
#'

fit.epsilon <- function(chunk.adv,freq=32, lower= 20,upper=80,diagnostic = FALSE){
  
  if ('velocity.X' %in% names(chunk.adv)){
    xts <- ts(chunk.adv$velocity.X, frequency=freq)
  } else {
    xts <- ts(chunk.adv$Up, frequency=freq)
  }
  
  w <- pwelch(xts, plot=FALSE)
  wavenum.spectra <- w$spec
  
  v.mn <- v.calc(chunk.adv)
  
  wavenum <- 2*pi*w$freq/v.mn # in radians/m
  lower.k = lower/v.mn
  upper.k = upper/v.mn
  
  
  use.i <- lower.k <= wavenum & wavenum <= upper.k
  
  pulse.averaging <- function(w.s,freq,del.t=1/32){
    
    denom <- (sin(pi*freq*del.t)/(pi*freq*del.t))^2
    
    pc.w = w.s/denom
    return(pc.w)  
  }
  
  eps <- function (w.s, k){
    
    in.bracket <- w.s*(k^(5/3))
    averaging <- in.bracket^(3/2)
    epsilon <- 1.04*mean(averaging)
    
    return(epsilon)
  }
  
  w.s <- pulse.averaging(wavenum.spectra,w$freq,del.t=1/freq)
  epsilon <- eps(w.s[use.i], k = wavenum[use.i])

  if (diagnostic){
    
    plot(wavenum,wavenum.spectra,log='xy')
    points(wavenum,w.s,col='red',cex=.2)
    abline(v=lower.k)
    abline(v=upper.k)
    lines(wavenum,0.52*(epsilon^(2/3))*wavenum^(-5/3),col='green')
  }
  
  return(epsilon)
}

rmse <- function(actual,predicted){
  rmse <- sqrt(sum((actual-predicted)^2)/length(actual))
  return(rmse)
}