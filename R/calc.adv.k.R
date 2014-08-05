calc.adv.k <- function(deploy.name="nv1/NV107", transform_coords = TRUE){
  
  
  require("GDopp")
  freq <- 32 # in Hz
  nu <- 0.2
  folder.nm <- '../../../Desktop/Science Projects/GDopp/supporting data/'
  file.nm <- paste0(deploy.name,'.dat')
  data.adv <- load.ADV(file.nm=file.nm, folder.nm =folder.nm)
  window.adv <- window_ADV(data.adv,freq=freq,window.mins=3)
  
  
  data.sen <- load.sen(file.nm=paste0(deploy.name,'.sen'), folder.nm)
  coord.df <- window_coord(data.sen, window.adv$window.idx, freq = freq)
  temp.df <- temp.calc(data.sen,window.adv$window.idx,freq=freq,calc.time=TRUE)
  temp.block <- temp.df$temperature
  temp.time <- temp.df$time
  num.wins <- length(temp.time)
  k.out <- vector(length=num.wins)
  trans_data <- c(-0.3462, 0.0869, 2.6611, -0.3252,  2.2522, -1.2607, -0.3616, -2.3228,-1.4019) #c(2896, 2896, 0, -2896, 2896, 0, -2896, -2896, 5792) / 4096
  trans_matrix <- matrix(data = trans_data, ncol = 3, byrow = TRUE) # get this from the file!!!!
  
  for (i in 1:num.wins){
    cat(i); cat(' of '); cat(num.wins); cat('\n')
    chunk.adv <- window.adv[window.adv$window.idx==i, ]
    
    tests <- c('frozen.turb.check_adv','beam.correlation.check_adv')
    #tests <- 'all'
    cck <- check.adv(chunk.adv=chunk.adv, tests, verbose=TRUE)
    
    
    
    if (!cck){
      if (transform_coords){
        chunk.adv <- coord_transform(trans_matrix, data_v = chunk.adv, position_data=coord.df[i, ])
      }
      epsilon <- fit.epsilon(chunk.adv, freq=freq,lower= 10, upper=50, diagnostic=T) #note ENU_adv now used here for coord flip
      k.out[i] <- epsilon2k(epsilon,temperature=temp.block[i],nu=nu) 
    } else {
      k.out[i] <- NA
    }
  }
  
  plot(temp.time,k.out,pch=1,main=deploy.name,ylim=c(0,30))
  return(data.frame('time'=temp.time,'k600'=k.out))
}

#calc.adv.k()