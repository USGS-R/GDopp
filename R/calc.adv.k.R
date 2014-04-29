calc.adv.k <- function(deploy.name='ALQ102'){
  
  freq <- 32
  nu <- 0.2
  folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
  file.nm <- paste0(deploy.name,'.dat')
  data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
  window.adv <- window.ADV(data.adv,freq=freq,window.mins=10)
  num.wins <- length(unique(window.adv$window.idx))
  
  data.sen <- load.sen(file.nm=paste0(deploy.name,'.sen'))
  temp.block <- temp.calc(data.sen,window.adv$window.idx,freq=freq)
  
  k.out <- vector(length=num.wins)
  
  for (i in 1:num.wins){
    chunk.adv <- window.adv[window.adv$window.idx==i, ]
    if (!check.adv(chunk.adv=chunk.adv)){
      epsilon <- fit.epsilon(chunk.adv,freq=freq)
      k.out[i] <- epsilon2k(epsilon,temperature=temp.block[i],nu=nu) 
    } else {
      k.out[i] <- NA
    }
  }
  
  plot(k.out,pch=1,main=deploy.name)
}

calc.adv.k()