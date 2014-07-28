
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
#' @references
#' Lohrmann, Atle, Ramon Cabrera, and Nicholas C. Kraus. \emph{Acoustic-
#' Doppler velocimeter (ADV) for laboratory use.} In Fundamentals and 
#' advancements in hydraulic measurements and experimentation, pp. 351-365. ASCE, 1994.
#' @export

coord_transform <- function(){
  # modified from transform.m
  hh = pi*(heading-90)/180
  pp = pi*pitch/180
  rr = pi*roll/180
  
  % Make heading matrix
  H = matrix(data = c(cos(hh), sin(hh), 0, -sin(hh), cos(hh), 0, 0, 0, 1), nrow = 3, ncol = 3)
  
  % Make tilt matrix
  
  p_data <- c(cos(pp), -sin(pp)*sin(rr), -cos(rr)*sin(pp), 0, cos(rr), -sin(rr),
              sin(pp), sin(rr)*cos(pp),  cos(pp)*cos(rr))
  P = matrix(data = p_data, nrow = 3, ncol = 3)
  
  % Make resulting transformation matrix
  R = H*P*T
  
  % Given beam velocities, ENU coordinates are calculated as
  enu = R*beam
  
  % Given ENU velocities, beam coordinates are calculated as
  beam = inv(R)*enu
  
  
  % Transformation between beam and xyz coordinates are done using
  % the original T matrix 
  xyz = T_org*beam
  beam = inv(T_org)*xyz
  
  % Given ENU velocities, xyz coordinates are calculated as
  xyz = T_org*inv(R)*enu
  
}
