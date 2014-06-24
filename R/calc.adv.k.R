calc.adv.k <- function(deploy.name='ALQ102'){
  
  require("GDopp")
  freq <- 32 # in Hz
  nu <- 0.2
  folder.nm <- '../../../Desktop/Science Projects/GDopp/supporting data/'
  file.nm <- paste0(deploy.name,'.dat')
  data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
  window.adv <- window_ADV(data.adv,freq=freq,window.mins=10)
  
  
  data.sen <- load.sen(file.nm=paste0(deploy.name,'.sen'))
  temp.df <- temp.calc(data.sen,window.adv$window.idx,freq=freq,calc.time=TRUE)
  temp.block <- temp.df$temperature
  temp.time <- temp.df$time
  num.wins <- length(temp.time)
  k.out <- vector(length=num.wins)
  
  for (i in 1:num.wins){
    cat(i); cat(' of '); cat(num.wins); cat('\n')
    chunk.adv <- window.adv[window.adv$window.idx==i, ]
    tests <- c('frozen.turb.check_adv','signal.noise.check_adv')
    #tests <- 'all'
    cck <- check.adv(chunk.adv=chunk.adv, tests, verbose=TRUE)
    if (!cck){
      epsilon <- fit.epsilon(chunk.adv,freq=freq,lower= 10,upper=50,diagnostic=TRUE)
      k.out[i] <- epsilon2k(epsilon,temperature=temp.block[i],nu=nu) 
    } else {
      k.out[i] <- NA
    }
  }
  
  plot(temp.time,k.out,pch=1,main=deploy.name,ylim=c(0,30))
}

calc.adv.k()