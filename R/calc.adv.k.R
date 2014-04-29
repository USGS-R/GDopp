calc.adv.k <- function(deploy.name='ALQ102'){
  
  folder.nm <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
  file.nm <- paste0(deploy.name,'.dat')
  data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
  window.adv <- window.ADV(data.adv,freq=32,window.mins=10)
  num.wins <- length(unique(window.adv$window.idx))
  
  k.out <- vector(length=num.wins)
  
  for (i in 1:num.wins){
    chunk.adv <- window.adv[window.adv$window.idx==i, ]
    if (!check.adv(chunk.adv=chunk.adv)){
      epsilon <- fit.epsilon(chunk.adv,freq=32)
      k.out[i] <- epsilon2k(epsilon,temperature=20,nu=0.2) # need true temperature!!
    } else {
      k.out[i] <- NA
    }
  }
  
  plot(k.out,pch=1,main=deploy.name)
}

#calc.adv.k()