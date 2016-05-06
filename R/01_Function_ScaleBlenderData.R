#' Scale blender data
#' 
#' Scales columns of a data frame according to a scale factor. For example, the
#' scale factor can be the diameter of a glue dot measured in blender. The
#' columns are rescaled accordingly to represent Millimeter instead of blender
#' units as measurement unit.
#' 
#' @param data Data frame containig the columns which should be rescaled.
#'   Remaining colums will be returned untouched.
#' @param colNames Character vector with names of the columns which should be
#'   rescaled.
#' @param scaleFactor Numeric variable containing the value of the real world
#'   object measured in blender. For example, this can be the diameter of a glue
#'   dot measured in blender (default = 8 mm). Therefore, scaling should be done
#'   person by person.
#' @param rwMeasure Optional. Real world measure of the object diamoter used to
#'   rescale. Default is the diameter of a glue dot of 8 millimeter.
#' @param verbose If TRUE, the function prints verbose output. Otherwise not.
#'   
#' @return Data frame with scaled columns.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' ScaleBlenderData()
#' 
#' @export
ScaleBlenderData <- function(data, colNames, scaleFactor, rwMeasure = 8, verbose = FALSE) {
    # Error handling
    if (!(is.data.frame(data))) {
        stop("Argument data does not contain a data frame!")
    }
    if (!(is.character(colNames))) {
        stop("Argument colNames is missing or not of type character!")
    }
    if (!(is.numeric(scaleFactor)) | (length(scaleFactor) != 1)) {
        stop("Argument scaleFactor is not of type numeric or contains more than one value!")
    }
    if (!(is.numeric(rwMeasure)) | (length(rwMeasure) != 1)) {
        stop("Argument rwMeasure is not of type numeric or contains more than one value!")
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    # Helper functions
    
    # Function Scale BU: Perform scaling via rule of proportion Example: Assuming the glue dot has 8 mm diameter (BU = blender units): (glue dot in BU /
    # 8mm) = (variable in BU / x mm) x = (variable in BU * 8 mm) / glue dot in BU
    
    scaleBU <- function(variableInBU) {
        x <- (variableInBU * rwMeasure)/scaleFactor
        return(x)
    }
    
    if (verbose) {
        writeLines("Perform scaling (may take a while).")
    }
    
    data[(names(data) %in% colNames)] <- scaleBU(data[(names(data) %in% colNames)])
    
    return(data)
} 
