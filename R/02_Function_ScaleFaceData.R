#' Scale head proportions
#' 
#' The heads of the participants are of different sizes. This function scales
#' and resizes the x/y values according to a standardized face.
#' 
#' @param data Data frame containig the columns which should be rescaled.
#'   Remaining colums will be returned untouched (e.g., z-values because we have
#'   no information to rescale them).
#' @param colNames Character vector with names of the columns which should be
#'   rescaled. Variable names containing vertical movement should end with '_x',
#'   whereas variable names containing horizontal movement should end with '_y'.
#' @param scaleFactor Numeric variable containing the value of the real world
#'   object measured in blender. For example, this can be the diameter of a glue
#'   dot measured in blender (default = 8 mm). Therefore, scaling should be done
#'   person by person.
#' @param pupilDist Single numeric value. Distance from left to right pupile
#'   measured in blender units
#' @param mouthcornerDist Single numeric value. Distance from left to right
#'   mouthcorner measured in blender units
#' @param leftPMDist Single numeric value. Distance from left pupile to left
#'   mouthcorner measured in blender units
#' @param rightPMDist Single numeric value. Distance from right pupile to right
#'   mouthcorner measured in blender units
#' @param rwMeasure Optional. Real world measure of the object diameter used to
#'   rescale. Default is the diameter of a glue dot of 8 millimeter.
#' @param verbose If TRUE, the function prints verbose output. Otherwise not.
#'   
#' @author Rainer Alexandrowicz, Axel Zinkernagel
#'   \email{rainer.alexandrowicz@aau.at, zinkernagel@uni-landau.de}
#'   
#' @examples
#' \dontrun{ScaleBlenderData()}
#' 
#' @return Data frame with scaled columns.
#'   
#' @export
ScaleFaceData <- function(data, colNames, scaleFactor, pupilDist, mouthcornerDist, leftPMDist, rightPMDist, rwMeasure = 8, verbose = FALSE) {
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
    if (!(is.numeric(pupilDist)) | (length(pupilDist) != 1)) {
        stop("Argument pupilDist is not of type numeric or contains more than one value!")
    }
    if (!(is.numeric(mouthcornerDist)) | (length(mouthcornerDist) != 1)) {
        stop("Argument mouthcornerDist is not of type numeric or contains more than one value!")
    }
    if (!(is.numeric(leftPMDist)) | (length(leftPMDist) != 1)) {
        stop("Argument leftPMDist is not of type numeric or contains more than one value!")
    }
    if (!(is.numeric(rightPMDist)) | (length(rightPMDist) != 1)) {
        stop("Argument rightPMDist is not of type numeric or contains more than one value!")
    }
    if (!(is.numeric(rwMeasure)) | (length(rwMeasure) != 1)) {
        stop("Argument rwMeasure is not of type numeric or contains more than one value!")
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    pupilDist_std <- pupilDist/rwMeasure  # scaling constant (1/8 mm)
    mouthcornerDist_std <- mouthcornerDist/rwMeasure
    leftPMDist_std <- leftPMDist/rwMeasure
    rightPMDist_std <- rightPMDist/rwMeasure
    
    scaleStd <- function(variableInBU) {
        x <- (variableInBU/rwMeasure)
        return(x)
    }
    
    if (verbose) {
        writeLines("Perform standardization of horizontal and vertical measures.")
    }
    
    # --- standardize horizontal and vertical measures
    
    data[(names(data) %in% colNames)] <- scaleStd(data[(names(data) %in% colNames)])
    
    # hh <- totalHeadHeight ww <- totalHeadWidth
    totalHeadWidth = 3 * pupilDist_std  # total height of head (using only pupil distance and not mouth corner distance, because mouth corner distance is interindividually more variable compared to pupil distance)
    totalHeadHeight = 3 * mean(leftPMDist_std, rightPMDist_std)  # total width of head (using both measures increases accuracy)
    
    # --- standardize participants (uniform)
    
    pupilDist_uni <- pupilDist_std/totalHeadWidth
    mouthcornerDist_uni <- mouthcornerDist_std/totalHeadWidth
    leftPMDist_uni <- leftPMDist_std/totalHeadHeight
    rightPMDist_uni <- rightPMDist_std/totalHeadHeight
    
    scaleUniHeight <- function(variableStdH) {
        x <- (variableStdH/totalHeadHeight)
        return(x)
    }
    
    scaleUniWidth <- function(variableStdW) {
        x <- (variableStdW/totalHeadWidth)
        return(x)
    }
    
    # Build vector containing names of horizontal and vertical variables
    colNamesHeight <- colNames[substr(colNames, nchar(colNames) - 1, nchar(colNames)) == "_y"]
    colNamesWidth <- colNames[substr(colNames, nchar(colNames) - 1, nchar(colNames)) == "_x"]
    
    if (verbose) {
        writeLines("Perform standardization of participants (uniform).")
    }
    
    data[(names(data) %in% colNamesHeight)] <- scaleUniHeight(data[(names(data) %in% colNamesHeight)])  # uniform head heigt
    data[(names(data) %in% colNamesWidth)] <- scaleUniWidth(data[(names(data) %in% colNamesWidth)])  # uniform head width
    
    return(data)
}
