#' Scale head proportions
#' 
#' The heads of the participants are of different sizes. This function rescales
#' the x/y values according to a standardized face. For resizing
#' the x-axis, the distance between the pupils of the subject is used. For resizing the y-axis,
#' the distance between the left eye and the left mouthcorner is used. 
#' Optionally, the distance between the mouthcorners for the x-axis, and the distance between
#' the right eye and the right mouthcorner for the y-axis may additionally be provided.
#' If provided, a mean of both measures is computed.
#' 
#' @param data Data frame containig the columns which should be rescaled.
#'   Remaining colums will be returned untouched (e.g., z-values because we have
#'   no information to rescale them).
#' @param colNames Character vector with names of the columns which should be
#'   rescaled. To differ between x-, and y-axis scaling, the variable names containing horizontal movement (left-right) should end with '_x',
#'   whereas variable names containing vertical movement (up-down) should end with '_y'.
#' @param colNameSubj Character vector with a single value containig the name 
#'   of the subject column of the data data-frame.
#' @param pupilDist Numeric vector of measurement of the distance from left to right pupil for each subject,
#'   measured in blender units.
#' @param leftPMDist Numeric vector of measuremet of the distance from left pupil to left
#'   mouthcorner for each subject, measured in blender units.
#' @param scaleFactor Numeric vector containing the values of the real world
#'   object measured in blender for each subject in the data data-frame. For 
#'   example, this can be the diameter of a glue dot measured in blender. 
#'   The scaleFactor vector must contain a scale value for each subject in the 
#'   data data-frame. The sorting of subjects in data data-frame and 
#'   scaleFactor vector must be the same. This factor is used to rescale the distances which are 
#'   measured in blender units.
#' @param mouthcornerDist Numeric vector of measurement of the distance from left to right
#'   mouthcorner for each subject measured in blender units.
#' @param rightPMDist Numeric vector of measurement of the distance from right pupil to right
#'   mouthcorner for each subject, measured in blender units.
#' @param rwMeasure Optional. Real world measure of the object diameter used to
#'   rescale. Default is the diameter of a glue dot of 8 millimeter.
#' @param verbose If TRUE, the function prints verbose output. Otherwise not.
#'   
#' @author Axel Zinkernagel, Rainer Alexandrowicz
#'   \email{zinkernagel@uni-landau.de, rainer.alexandrowicz@aau.at}
#'   
#' @examples
#' \dontrun{scaleFaceData()}
#' 
#' @return Data frame with scaled columns.
#'   
#' @export
scaleFaceData <- function(data, colNames, colNameSubj, pupilDist, leftPMDist, scaleFactor, mouthcornerDist = NA, rightPMDist = NA, rwMeasure = 8, 
    verbose = FALSE) {
    # Error handling
    if (!(is.data.frame(data))) {
        stop("Argument data does not contain a data frame!")
    }
    if (!(is.character(colNames))) {
        stop("Argument colNames is missing or not of type character!")
    }
    if (!(is.character(colNameSubj))) {
        stop("Argument colNameSubj is missing or not of type character!")
    }
    if (!(is.numeric(pupilDist)) | !(length(pupilDist) > 2)) {
        stop("Argument pupilDist is not of type numeric or contains only one value!")
    }
    if (!(is.numeric(leftPMDist)) | !(length(leftPMDist) > 2)) {
        stop("Argument leftPMDist is not of type numeric or contains only one value!")
    }
    if (!(is.numeric(scaleFactor)) | !(length(scaleFactor) > 2)) {
        stop("Argument scaleFactor is not of type numeric or contains only one value!")
    }
    if (!is.na(mouthcornerDist[1])) {
        if (!(is.numeric(mouthcornerDist)) | !(length(mouthcornerDist) > 2)) {
            stop("Argument mouthcornerDist is not of type numeric or contains only one value!")
        }
    }
    if (!is.na(rightPMDist[1])) {
        if (!(is.numeric(rightPMDist)) | !(length(rightPMDist) > 2)) {
            stop("Argument rightPMDist is not of type numeric or contains only one value!")
        }
    }
    if (!(is.numeric(rwMeasure)) | (length(rwMeasure) != 1)) {
        stop("Argument rwMeasure is not of type numeric or contains more than one value!")
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    # Determing number of subjects in data
    subjects <- unique(data[[colNameSubj]])
    
    # Principle of rescaling (again): Perform scaling via rule of proportion.  Example: Assuming the glue dot has 8 mm diameter (BU = blender
    # units): (glue dot in BU / 8mm) = (variable in BU / x mm) x = (variable in BU * 8 mm) / glue dot in BU
    
    # Compute real world measures for eye-eye distance (x-axis) and eye-mouthcorner distance to get mean and sd
    sFxyaxes <- as.data.frame(matrix(data = NA, nrow = length(subjects), ncol = 2))
    colnames(sFxyaxes) <- c("x", "y")
    
    for (i in 1:length(subjects)) {
        if (verbose) {
            writeLines(paste("Aggregating facial measures of subject ", i, " of ", length(subjects), ".", sep = ""))
        }
        # Scale distances (eye-eye, left eye-left mouth, etc., to real world measures) by scale factor (sF)
        sFrwMeasure <- rwMeasure/scaleFactor[i]
        
        # Scale x-axis
        if (is.na(mouthcornerDist[1])) {
            sFxaxis <- pupilDist[i] * sFrwMeasure
        } else {
            sFxaxis <- mean(c(pupilDist[i], mouthcornerDist[i])) * sFrwMeasure
        }
        
        # Scale y-axis
        if (is.na(rightPMDist[1])) {
            sFyaxis <- leftPMDist[i] * sFrwMeasure
        } else {
            sFyaxis <- mean(c(leftPMDist[i], rightPMDist[i])) * sFrwMeasure
        }
        sFxyaxes[i, "x"] <- sFxaxis
        sFxyaxes[i, "y"] <- sFyaxis
    }
    
    meanEEdist <- mean(sFxyaxes$x)
    sdEEdist <- sd(sFxyaxes$x)
    meanEMdist <- mean(sFxyaxes$y)
    sdEMdist <- sd(sFxyaxes$y)
    
    writeLines(paste("\nMean eye-eye distance is ", round(meanEEdist, 2), "mm (SD: ", round(sdEEdist, 2), ").", sep = ""))
    writeLines(paste("\nMean eye-mouthcorner distance is ", round(meanEMdist, 2), "mm (SD: ", round(sdEMdist, 2), ").", sep = ""))
    
    # individual values are now set in relation to mean value which results in the individual scale factors for x and y-axes
    for (i in 1:length(subjects)) {
        if (verbose) {
            writeLines(paste("Scaling subject ", i, " of ", length(subjects), ".", sep = ""))
        }
        
        # compute individual scale factor
        sF2x <- sFxyaxes[i, "x"]/meanEEdist
        sF2y <- sFxyaxes[i, "y"]/meanEMdist
        
        # rescale individual measures to standard face measures
        data[data$subject == subjects[i], colNames[grep("_x", colNames)]] <- data[data$subject == subjects[i], colNames[grep("_x", colNames)]]/sF2x
        data[data$subject == subjects[i], colNames[grep("_y", colNames)]] <- data[data$subject == subjects[i], colNames[grep("_y", colNames)]]/sF2y
    }
    return(data)
}
