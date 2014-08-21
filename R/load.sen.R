#'@title load sensor diagnostic data
#'@description 
#'Assumes the following format: \cr
#'\tabular{rlll}{
#'1 \tab Month   \tab (1-12) \cr
#'2 \tab Day  \tab (1-31)\cr
#'3 \tab Year \tab \cr
#'4 \tab Hour \tab (0-23)\cr
#'5 \tab Minute    \tab (0-59)\cr
#'6 \tab Second  \tab (0-59) \cr
#'7 \tab Error code  \tab  \cr
#'8 \tab Status code  \tab  \cr
#'9 \tab Battery voltage  \tab (V) \cr
#'10 \tab Speed of sound  \tab (m/s) \cr
#'11 \tab Heading  \tab (deg) \cr
#'12 \tab Pitch  \tab (deg) \cr
#'13 \tab Roll  \tab (deg) \cr
#'14 \tab Temperature  \tab (degC) \cr
#'15 \tab Analog input \tab  \cr
#'16 \tab Checksum \tab (1=failed)\cr
#'}
#'
#'@details a \code{GDopp} function for loading sensor diagnostic data from a file.\cr 

#'@param file.nm a valid file name.
#'@param folder.nm the folder where the data file resides.
#'@return a GDopp data.frame.
#'@keywords load.sen
#'@examples 
#'folder.nm <- system.file('extdata', package = 'GDopp') 
#'file.nm <- "ALQ102.sen"
#'data.sen <- load_sen(file.nm = file.nm, folder.nm = folder.nm)
#'@export
#'
load_sen <- function(file.nm, folder.nm){
  adv.sen.names <- c('month','day','year','hour','minute','second',
                     'error.cd','status.cd','batt.v','sound.speed',
                     'heading','pitch','roll','temperature','analog.in','checksum')
  
  data.sen <- read.table(file.path(folder.nm,file.nm))
  
  names(data.sen) <- adv.sen.names
  
  return(data.sen)
}