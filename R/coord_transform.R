
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
#' @param data_v a velocity matrix (a data.frame)
#' @param position_data positional data as a data.frame
#' @author
#' Jordan S. Read
#' @references
#' Lohrmann, Atle, Ramon Cabrera, and Nicholas C. Kraus. \emph{Acoustic-
#' Doppler velocimeter (ADV) for laboratory use.} In Fundamentals and 
#' advancements in hydraulic measurements and experimentation, pp. 351-365. ASCE, 1994.
#' @examples
#' trans_data <- c(-0.3462, 0.0869, 2.6611, -0.3252,  2.2522, -1.2607, -0.3616, -2.3228,-1.4019) # trans matrix for example package data
#' trans_matrix <- matrix(data = trans_data, ncol = 3, byrow = TRUE) 
#' position_data <- data.frame(heading = 358.4, pitch = -1.38, roll = -0.3)
#' data_v <- data.frame(velocity.X =  c(-0.205, -0.185), 
#' # X is up, Y is along stream, Z is cross stream
#' velocity.Y = c(-1.5303, -1.5452), velocity.Z = c(0.3747, 0.3763))
#' ENU <- coord_transform(trans_matrix, data_v, position_data)
#' @export

coord_transform <- function(trans_matrix, data_v, position_data){
  
  # does it handle is.null(dim)??
  x <- data_v$velocity.X
  y <- data_v$velocity.Y
  z <- data_v$velocity.Z
  xyz <- matrix(data=c(y, z, x), ncol = 3) # flipped because transform is based on z=up, x=alongstream, y=cross-stream
  heading <- position_data$heading
  pitch <- position_data$pitch
  roll <- position_data$roll 
  res_trans <- resultant_trans(trans_matrix, heading, pitch, roll)

  ENU <- apply(X = t(xyz), MARGIN = 2, FUN = xyz_2_enu, 
               trans_matrix = trans_matrix, res_trans = res_trans)
  
  ENU.df <- data.frame(t(ENU))
  names(ENU.df) <- c('East','North','Up')
  return(ENU.df)
}

resultant_trans <- function(trans_matrix, heading, pitch, roll){
  
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
  
  return(R)
}

xyz_2_enu <- function(trans_matrix, res_trans, xyz){
  
  beam <- xyz_2_beam(trans_matrix, xyz)
  
  # Given beam velocities, ENU coordinates are calculated as
  enu <- beam_2_enu(res_trans, beam)
  # same as solve(trans_matrix) %*% res_trans %*% xyz ????
  return(enu)
}

enu_2_xyz <- function(trans_matrix, res_trans, enu){
  
  xyz <- trans_matrix %*% solve(res_trans) %*% enu
  
  return(xyz)
}

beam_2_enu <- function(res_trans, beam){
  
  enu <- res_trans %*% beam
  
  return(enu)
}

beam_2_xyz <- function(trans_matrix, beam){
  xyz <- trans_matrix %*% beam
}

xyz_2_beam <- function(trans_matrix, xyz){
  
  beam <- solve(trans_matrix) %*% xyz
  return(beam)
}

enu_2_beam <- function(res_trans, enu){
  
  beam <- solve(res_trans) %*% enu
  return(beam)
  
}