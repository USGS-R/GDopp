#'@title get kinematic viscosity
#'@param temperature Temperature in degC of water
#'@return kinematic viscosity
#'@keywords kinematic.vis
#'@examples 
#'kin_vis <- get_kin_viscosity(temperature = 20)
#'@export

get_kin_viscosity <- function(temperature=20) {
  # from Mays 2005, Water Resources Engineering
  tempTable <- seq(0,100,by=5)
  # table in m2/s E-6
  visTable <- c(1.792,1.519,1.308,1.141,1.007,0.897,
                0.804,0.727,0.661,0.605,0.556,0.513,0.477,0.444,
                0.415,0.39,0.367,0.347,0.328,0.311,0.296)
  v <- data.frame(approx(tempTable,visTable,xout = temperature))[2]
  v <- v*1e-6
  return(v$y)
}