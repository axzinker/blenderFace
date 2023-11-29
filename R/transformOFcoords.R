#' Transform OpenFace coordinates
#' 
#' OpenFace (\url{https://github.com/TadasBaltrusaitis/OpenFace/}) allows
#' to export and save x, y, and z-coordinates per frame of a given set of markers. 
#' However, these coordinates are confounded by head movements and head turns. 
#' The function `transformOFcoords` allows to transform the OpenFace coordinate 
#' system into the R coordinate system (flip y and z axes, so that the face is
#' looking at you) and controls for head movements and rotations (using pose_Tx, 
#' pose_Ty, pose_Tz, pose_Rx, pose_Ry, pose_Rz; see 
#' \code{\link{https://github.com/TadasBaltrusaitis/OpenFace}}).
#' 
#' @param df Data frame containing OpenFace output CSV with x,y,z marker columns 
#' and framerows.
#' 
#' @param x String containing the name of the column containig the x-coordinate
#' values of the marker (e.g., X_48).
#' 
#' @param y String containing the name of the column containig the y-coordinate
#' values of the marker (e.g., Y_48).
#'
#' @param z String containing the name of the column containig the z-coordinate
#' values of the marker (e.g., Z_48).
#'
#' @param pose_Tx String containing the name of the column containig the x-coordinate
#' values of the head movement (e.g., pose_Tx).
#' 
#' @param pose_Ty String containing the name of the column containig the y-coordinate
#' values of the head movement (e.g., pose_Ty).
#' 
#' @param pose_Tz String containing the name of the column containig the z-coordinate
#' values of the head movement (e.g., pose_Tz).
#' 
#' @param pose_Rx String containing the name of the column containig the x-values 
#' of the head rotation in radiant (e.g., pose_Rx).
#' 
#' @param pose_Ry String containing the name of the column containig the y-values 
#' of the head rotation in radiant (e.g., pose_Ry).
#' 
#' @param pose_Rz String containing the name of the column containig the z-values 
#' of the head rotation in radiant (e.g., pose_Rz).
#'   
#' @param transfToRcoord Logical value if OpenFace coordinates should be transformed
#' into the R coordinate system. This means that the Y and the Z axis are reverted. 
#' Default is TRUE.
#' 
#' @param transpose Logical value if the head movements should be controlled for.
#' The coordinates of the head pose_Tx, pose_Ty and pose_Tz are subtracted from
#' the marker coordinates. Default is TRUE.
#' 
#' @param rotate Logical value if the head rotation should be controlled for.
#' The rotation of the head pose_RTx, pose_Ry and pose_Rz are eleminated from
#' the marker coordinates. Default is TRUE.
#'   
#' @return Dataframe with transformed OpenFace coordinates.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-wuppertal.de}
#'   
#' @examples
#' \dontrun{
#' axtestRot <- transformOFcoords(axtest, x = "X_48", y = "Y_48", z = "Z_48",
#' pose_Tx = "pose_Tx", pose_Ty = "pose_Ty", pose_Tz = "pose_Tz", 
#' pose_Rx = "pose_Rx", pose_Ry = "pose_Ry", pose_Rz = "pose_Rz")
#' }
#'   
#' @export
transformOFcoords <- function(df, x, y, z, pose_Tx, pose_Ty, pose_Tz, 
                              pose_Rx, pose_Ry, pose_Rz, transfToRcoord = TRUE, 
                              transpose = TRUE, rotate = TRUE) {
  
  # for error diagnosis, perform each axis rotation separately
  rotX = TRUE
  rotY = TRUE
  rotZ = TRUE
  
  if (transpose) {  
    # subtrackt head movements from marker movents
    df[x] <- df[x] - df[pose_Tx]
    df[y] <- df[y] - df[pose_Ty]
    df[z] <- df[z] - df[pose_Tz]
  }
  
  # invert head rotation to eliminate head rotation in marker movements
  df[pose_Rx] <- df[pose_Rx] * (-1)
  df[pose_Ry] <- df[pose_Ry] * (-1)
  df[pose_Rz] <- df[pose_Rz] * (-1)
  
  # initiate output matrix, memory preallocation is faster
  output <- as.data.frame(matrix(data = NA, ncol = 3, nrow = nrow(df)))
  names(output) <- c(x,y,z)
  
  # Axel fix me: this loop is slow and should be converted to lapply
  for (i in 1:nrow(df)) { # loop over every row in df
    
    # compute rotation matrices
    MatRotX <- matrix(c(1, 0, 0,
                        0, cos(df[i,pose_Rx]), -sin(df[i,pose_Rx]),
                        0, sin(df[i,pose_Rx]), cos(df[i,pose_Rx])
    ), ncol = 3, nrow = 3, byrow = TRUE)
    
    MatRotY <- matrix(c(cos(df[i,pose_Ry]), 0, sin(df[i,pose_Ry]),
                        0, 1, 0,
                        -sin(df[i,pose_Ry]), 0, cos(df[i,pose_Ry])
    ), ncol = 3, nrow = 3, byrow = TRUE)
    
    MatRotZ <- matrix(c(cos(df[i,pose_Rz]), -sin(df[i,pose_Rz]), 0,
                        sin(df[i,pose_Rz]), cos(df[i,pose_Rz]), 0,
                        0, 0, 1
    ), ncol = 3, nrow = 3, byrow = TRUE)
    
    # perform matrix multiplications
    # according to OpenFace documentation (https://github.com/TadasBaltrusaitis/OpenFace/wiki/Output-Format)
    # MatRotX: pitch
    # MatRotY: yaw
    # MatRotZ: role
    # SO(3): yaw %*% pitch %*% roll

    # for error diagnosis, perform each axis rotation separately
    tmpRot <- c(df[i,x], df[i,y], df[i,z])
    if (rotate) {
      if (rotX) {
        tmpRot <- MatRotX %*%  tmpRot
      }
      if (rotY) {
        tmpRot <- MatRotY %*%  tmpRot
      }
      if (rotZ) {
        tmpRot <- MatRotZ %*%  tmpRot
      }
    }
    
    xyzRot <- tmpRot
    
    # attach to output matrix
    output[i,1] <- xyzRot[1]
    output[i,2] <- xyzRot[2]
    output[i,3] <- xyzRot[3]
  }
  
  if (transfToRcoord) {
    # revert Y axis
    output[,2] <- output[,2] * (-1)
    # revert Z axis
    output[,3] <- output[,3] * (-1)
  }
  
  return(output)
}

