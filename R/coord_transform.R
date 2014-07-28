
#' @title coordinate transformation
#' @description
#' coord_transform is modified from Transform.m (from http://www.nortek-
#' as.com/en/knowledge-center), which is a Matlab script that shows how 
#' velocity data can be transformed between beam coordinates and ENU coordinates. 
#' Beam coordinates are defined as the velocity measured along the three 
#' beams of the instrument.
#' ENU coordinates are defined in an earth coordinate system, where
#' E represents the East-West component, N represents the North-South
#' component and U represents the Up-Down component.
#'
#' Note that the transformation matrix must be recalculated every time
#' the orientation, heading, pitch or roll changes.
#' @param trans_matrix a matrix of tranformation values (static) in 3 x 3 matrix. 
#' @references
#' Lohrmann, Atle, Ramon Cabrera, and Nicholas C. Kraus. \emph{Acoustic-
#' Doppler velocimeter (ADV) for laboratory use.} In Fundamentals and 
#' advancements in hydraulic measurements and experimentation, pp. 351-365. ASCE, 1994.
#' @export

coord_transform <- function(trans_matrix, data_v, position_data){
  
  x <- data_v$velocity.X
  y <- data_v$velocity.Y
  z <- data_v$velocity.Z
  beam <- matrix(data=c(x, y, z), ncol = 3)

  hh <- pi * (heading - 90) / 180
  pp <- pi * pitch / 180
  rr <- pi * roll / 180
  
  # Make heading matrix
  H <- t(matrix(data = c(cos(hh), sin(hh), 0, -sin(hh), cos(hh), 0, 0, 0, 1), ncol = 3))
  
  # Make tilt matrix
  p_data <- c(cos(pp), -sin(pp)*sin(rr), -cos(rr)*sin(pp), 0, cos(rr), -sin(rr),
              sin(pp), sin(rr)*cos(pp),  cos(pp)*cos(rr))
  P <- t(matrix(data = p_data, nrow = 3, ncol = 3))
  
  # Make resulting transformation matrix
  R <- H %*% P %*% trans_matrix
  
  # Given beam velocities, ENU coordinates are calculated as
  enu <- R %*% beam

  
  
  # Transformation between beam and xyz coordinates are done using
  # the original T matrix 
  xyz = T %*% beam
  beam = solve(T) %*% xyz
  
  # Given ENU velocities, xyz coordinates are calculated as
  xyz = T_org*inv(R)*enu
  
}
