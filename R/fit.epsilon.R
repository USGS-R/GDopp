#'@title fit epsilon from block of ADV data
#'@description 
#'solves for epsilon from ADV data in the inertial subrange assuming Zappa et al. 2003  \cr
#'
#'@details a \code{velocimeterK} function for splitting sensor data into temporal segments.\cr 
#'
#'@references
#'Zappa, Christopher J., Peter A. Raymond, Eugene A. Terray, and Wade R. McGillis. 
#'"Variation in surface turbulence and the gas transfer velocity over a tidal cycle in a macro-tidal estuary." 
#'Estuaries 26, no. 6 (2003): 1401-1415.
#'
#'@param \code{chunk.adv} a data.frame created with load.ADV, with the window.idx column
#'@param \code{freq} a numeric, in Hz, that represents the input data
#'@param \code{lower} a numeric, in Hz, that represents the lower bounds for range of epsilon calculation
#'@param \code{upper} a numeric, in Hz, that represents the upper bounds for range of epsilon calculation
#'@param \code{diagnostic} a boolean for diagnostic plot
#'@return a single value of epsilon estimating from the temporal block given. 
#'@keywords fit.epsilon
#'@import oce
#'@examples 
#'folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
#'file.nm <- "ICACOS04.dat"
#'data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
#'fit.epsilon(window.adv[window.adv$window.idx==7, ],freq=32)
#'@export
#'

fit.epsilon <- function(chunk.adv,freq=32, lower= 20,upper=80,diagnostic = FALSE){
  
  xts <- ts(chunk.adv$velocity.Z, frequency=freq)
  w <- pwelch(xts, plot=FALSE)
  wavenum.spectra <- w$spec
  
  v.mn <- v.calc(chunk.adv)
  
  wavenum <- 2*pi*w$freq/v.mn # in radians/m
  lower.k = lower/v.mn
  upper.k = upper/v.mn
  
  use.i <- lower.k <= wavenum & wavenum <= upper.k
  
  f <- function (x, w.s, k){
    
    #eps.2.3 <- x^(2/3)
    e.log = x
    epsilon.rmse <- rmse(log(0.52*k^(-5/3))+e.log,log(w.s))
    
    return(epsilon.rmse)
  }
  
  epsilon.2.3.log <- optimize(f, c(-100,1000), tol = 0.0001, w.s = wavenum.spectra[use.i], k = wavenum[use.i])$minimum
  
  epsilon = (exp(epsilon.2.3.log))^3/2
  if (diagnostic){
    
    plot(wavenum,wavenum.spectra,log='xy')
    abline(v=lower.k)
    abline(v=upper.k)
    eps.2.3 = epsilon^(2/3)
    lines(wavenum,0.52*eps.2.3*wavenum^(-5/3),col='green')
  }
  
  return(epsilon)
}

rmse <- function(actual,predicted){
  rmse <- sqrt(sum((actual-predicted)^2)/length(actual))
  return(rmse)
}