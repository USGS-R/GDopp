#'@title checks adv
#'@description 
#'checks data for various quality metrics  \cr
#'
#'@details a \code{GDopp} function for checking data quality.\cr 
#'
#'@param chunk.adv a data.frame created with load.ADV, with the window.idx column
#'@param tests a character array of test names, or 'all' to run all tests
#'@param verbose boolean for diagnostic print outs
#'@return failed, T or F
#'@keywords methods, math
#'@references
#'Vachon, Dominic, Yves T. Prairie, and Jonathan J. Cole. 
#'\emph{The relationship between near-surface turbulence and gas transfer 
#'velocity in freshwater systems and its implications for floating chamber measurements of gas exchange}. 
#'Limnology and Oceanography 55, no. 4 (2010): 1723.
#'
#'Kitaigorodskii, S. A., M. A. Donelan, J. L. Lumley, and E. A. Terray. 
#'\emph{Wave-turbulence interactions in the upper ocean. Part II. Statistical 
#'characteristics of wave and turbulent components of the random velocity 
#'field in the marine surface layer}. Journal of Physical Oceanography 13, no. 11 (1983): 1988-1999.
#'
#'#'Lien, Ren-Chieh, and Eric A. D'Asaro. \emph{Measurement of turbulent kinetic energy dissipation rate with a Lagrangian float.}
#' Journal of Atmospheric and Oceanic Technology 23, no. 7 (2006): 964-976.
#'
#'@examples 
#'\dontrun{
#'folder.nm  <- system.file('extdata', package = 'GDopp')
#'file.nm <- "ALQ102.dat"
#'data.adv <- load_adv(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window_adv(data.adv,freq=32,window.mins=10)
#'chunk.adv <- window.adv[window.adv$window.idx==7, ]
#'check.adv(chunk.adv,tests=c('signal.noise.check_adv','frozen.turb.check_adv'),verbose=TRUE)
#'}
#'@export
check_adv <- function(chunk.adv,tests='all', verbose=FALSE){
  
  if (is.null(tests)){stop("cannot perform check without any tests specified. use \"all\" for all tests")}
  
  pub.fun <- ls(getNamespace("GDopp"), all.names=TRUE)
  pos.tests <- pub.fun[grepl('check_adv?', pub.fun, ignore.case=TRUE)]
  if (tests[1]=='all'){
    tests = pos.tests
  }

  fails = vector(length = length(tests))
  
  for (i in seq_len(length(tests))){
    fails[i] = tryCatch({
      do.call(match.fun(tests[i]),list(chunk.adv=chunk.adv))
    }, error = function(e) {
      test.try <- paste(pos.tests,collapse = '\n')
      stop(paste0('adv check for test name "',tests[i],'"" not found, try\n',test.try))
    })
  }
  
  if (verbose){
    dots <- get.dots(tests)
    for (i in seq_len(length(tests))){
      cat(tests[i]);cat(dots[i]);
      cat(ifelse(fails[i],'failed\n','passed\n'))
    }
  }
  
  failed <- ifelse(any(fails),TRUE,FALSE)
  
  return(failed)
  
}
#'@export
signal.noise.check_adv <- function(chunk.adv){

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

#'@references
#'Lien, Ren-Chieh, and Eric A. D'Asaro. \emph{Measurement of turbulent kinetic energy dissipation rate with a Lagrangian float.}
#' Journal of Atmospheric and Oceanic Technology 23, no. 7 (2006): 964-976.
#'@export
beam.correlation.check_adv <- function(chunk.adv,threshold = 90){
  x1 = mean(chunk.adv$correlation.X,na.rm = T)
  x2 = mean(chunk.adv$correlation.Y,na.rm = T)
  x3 = mean(chunk.adv$correlation.Z,na.rm = T)
  
  failed = ifelse(any(c(x1,x2,x3) < threshold), TRUE, FALSE)
  return(failed)
  #Bursts were discarded if the average correlation of any of three ADV beams was lower than 0.9
}
#'@export
frozen.turb.check_adv <- function(chunk.adv){
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

get.dots <- function(chars.in){
  
  len_char <- nchar(chars.in)
  mx.char <- max(len_char)
  dots.out <- rep('',length(chars.in))
  for (i in seq_len(length(chars.in))){
    num.dots <- mx.char-len_char[i]+3
    dots.out[i] <- paste(rep('.',num.dots),collapse='')
  }
  return(dots.out)
}