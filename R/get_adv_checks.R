#'@title returns a list of possible check functions for the adv
#'@description 
#'checks data for various quality metrics  \cr
#'
#'@details This function figures out what the valid arguments for the \code{check_adv()} function can be. 
#'This is useful for both generating a list of tests, or passing in a character array of valid function 
#'names to \code{check_adv()}.
#'
#'@return a character array of valid check functions
#'@keywords methods, math
#'@examples 
#'\dontrun{
#'folder.nm  <- system.file('extdata', package = 'GDopp')
#'file.nm <- "ALQ102.dat"
#'data.adv <- load_adv(file.nm=file.nm, folder.nm =folder.nm)
#'window.adv <- window_adv(data.adv,freq=32,window.mins=10)
#'chunk.adv <- window.adv[window.adv$window.idx==7, ]
#'pos_tests <- get_adv_checks()
#'print(pos_tests)
#'check_adv(chunk.adv,tests=pos_tests[1:2],verbose=TRUE)
#'}
#'@export
get_adv_checks <- function(){
  pub.fun <- ls(getNamespace("GDopp"), all.names=TRUE)
  pos.tests <- pub.fun[grepl('_check_adv?', pub.fun, ignore.case=TRUE)]
  return(pos.tests)
}