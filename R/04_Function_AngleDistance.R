#' Computes distance and angle from marker columns
#' 
#' Computes the distance and the angle for x/y marker movement. Data is supposed
#' to start at x = 0, y =0, use function CenterCond before.
#' 
#' @param x Numeric vector of x values of x marker movement, beginnig at 0
#' @param y Numeric vector of y values of y marker movement, beginnig at 0
#' @param verbose If TRUE, the function prints verbose output. Otherwise not.
#'   
#' @return Numeric vector with median distance and angle.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' AngleDistance(x,y)
#' 
#' @export
AngleDistance <- function(x, y, verbose = FALSE) {
    # Error handling
    if (!(is.numeric(x)) | (length(x) != 1)) {
        stop("Argument x is missing, not of type numeric or contains more than one element!")
    }
    if (!(is.numeric(y)) | (length(y) != 1)) {
        stop("Argument y is missing, not of type numeric or contains more than one element!")
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    getDistance <- function(Point1, Point2) {
        # Computes distance between two points in a coordinate system Its a two dimensional distance (not eukledian)
        return(sqrt((Point2[1] - Point1[1])^2 + (Point2[2] - Point1[2])^2))
    }
    
    getAngle <- function(x, y) {
        # Computes the angle between the median of data points and origin of coordinate system Division by (pi/180) necessary to get deg (not rad)
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
    
    return(c(getDistance(c(0, 0), c(x, y)), getAngle(x, y)))
} 
