#' Computes distance and angle from marker columns
#' 
#' Computes the distance and the angle for a (mean, median) x/y marker 
#' point. The distance and the angle is computed from x = 0, y = 0 to the 
#' x/y - values given. Therefore, to get correct values for distance and angle
#' use the function CenderCond() before, to set the x/y-start values for each 
#' condition to x = 0, y = 0.
#' 
#' @param x Numeric vector of x values of x marker movement, beginnig at 0
#' @param y Numeric vector of y values of y marker movement, beginnig at 0
#' @param verbose If TRUE, the function prints verbose output. Otherwise not. 
#' Default is FALSE.
#'   
#' @return Numeric vector with median distance and angle.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' \dontrun{angleDistance(x,y)}
#' 
#' @export
angleDistance <- function(x, y, verbose = FALSE) {
    # Error handling
    if (!(is.numeric(x)) | (length(x) != 1)) {
        stop("Argument x is missing, not of type numeric, or contains more than one element!")
    }
    if (!(is.numeric(y)) | (length(y) != 1)) {
        stop("Argument y is missing, not of type numeric, or contains more than one element!")
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    getDistance <- function(Point1, Point2) {
        # Computes distance between two points in a coordinate system Its a two dimensional distance (not eukledian)
        return(sqrt((Point2[1] - Point1[1])^2 + (Point2[2] - Point1[2])^2))
    }
    
    getAngle <- function(x, y) {
        # Computes the angle between the median of data points and origin of coordinate system Division by (pi/180) necessary to get deg (not
        # rad)
        angle <- NA
        # catch error, if value is NA
        if (!is.na(x) & !is.na(y)) {
            if ((atan2(y, x)/(pi/180)) < 0) {
                angle <- 360 + (atan2(y, x)/(pi/180))
            } else {
                angle <- atan2(y, x)/(pi/180)
            }
        }
        return(angle)
    }
    
    return(c(getAngle(x, y), getDistance(c(0, 0), c(x, y))))
}
