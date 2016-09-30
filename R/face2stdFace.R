#' Scale head proportions
#' 
#' The heads of the participants are of different sizes. This function rescales
#' the x/y values according to a standardized face. The standardized face is of 
#' height 1 and width 1, while the pupil-pupil distance is defined as 1/3 and the 
#' left-pupil -- left-mouth-corner distance is defined as 1/3. Accordongly, for resizing
#' the x-axis, the distance between the pupils of the subject is used. For resizing the y-axis,
#' the distance between the left pupil and the left mouthcorner is used. 
#' Optionally, the distance between the mouthcorners for the x-axis, and the distance between
#' the right pupil and the right mouthcorner for the y-axis may additionally be provided.
#' If provided, a mean of both measures is computed. Be aware, if you previously rescaled 
#' the data into millimeter by the function \code{\link{bu2mm}}, you also have to
#' rescale the scaling parameters e.g., the pupil-pupil distance into millimeter first
#' and use the millimeter values as scaling parameters!
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
#' @param mouthcornerDist Numeric vector of measurement of the distance from left to right
#'   mouthcorner for each subject measured in blender units.
#' @param rightPMDist Numeric vector of measurement of the distance from right pupil to right
#'   mouthcorner for each subject, measured in blender units.
#' @param verbose If TRUE, the function prints verbose output. Otherwise not.
#'   
#' @author Axel Zinkernagel, Rainer Alexandrowicz
#'   \email{zinkernagel@uni-landau.de, rainer.alexandrowicz@aau.at}
#'   
#' @examples
#' \dontrun{face2stdFace()}
#' 
#' @return Data frame with scaled columns.
#'   
#' @export
face2stdFace <- function(data, colNames, colNameSubj, pupilDist, leftPMDist, mouthcornerDist = NA, rightPMDist = NA, rwMeasure = 8, 
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
    if (!(is.numeric(pupilDist)) | !(length(pupilDist) >= 2)) {
        stop("Argument pupilDist is not of type numeric or contains only one value!")
    }
    if (!(is.numeric(leftPMDist)) | !(length(leftPMDist) >= 2)) {
        stop("Argument leftPMDist is not of type numeric or contains only one value!")
    }
    if (!is.na(mouthcornerDist[1])) {
        if (!(is.numeric(mouthcornerDist)) | !(length(mouthcornerDist) >= 2)) {
            stop("Argument mouthcornerDist is not of type numeric or contains only one value!")
        }
    }
    if (!is.na(rightPMDist[1])) {
        if (!(is.numeric(rightPMDist)) | !(length(rightPMDist) >= 2)) {
            stop("Argument rightPMDist is not of type numeric or contains only one value!")
        }
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    # Helper functions
    fcat <- function(...,newline=TRUE) {if (newline) cat(...,"\n") else cat(...); flush.console() }  # immediate console output
    
    # Determing number of subjects in data
    subjects <- unique(data[[colNameSubj]])
    
    # Principle of rescaling: Perform scaling via rule of proportion (BU = blender units).
    # For example scaling the x-axis:
    # ((pupil-pupil distance in BU)/(1/3)) = Factor to divide BU by, to obtain the standardized measure,
    # when 1/3 is the standardized pupil-pupil distance
    # For example, the pupil-pupil distance is measured in Blender with 1 BU:
    # ((1 BU)/(1/3)) = 3
    # ((2 BU)/ 3) = 2/3
    # Analog for left-pupil--left-mouthcorner distance for scaling the y-axis

    # individual values are now set in relation to mean value which results in the individual scale factors for x and y-axes
    for (i in 1:length(subjects)) {
        if (verbose) {
            fcat(paste("Standardizing subject ", i, " of ", length(subjects), ".", sep = ""))
        }
        
        # compute individual scale factor for x-, and y-axis
        isF_x <- pupilDist[i] * 3 # variable /(1/3) == variable * 3
        isF_y <- leftPMDist[i]* 3 # variable /(1/3) == variable * 3
        
        # rescale individual measures to standard face measures
        data[data$subject == subjects[i], colNames[grep("_x", colNames)]] <- data[data$subject == subjects[i], colNames[grep("_x", colNames)]] / isF_x
        data[data$subject == subjects[i], colNames[grep("_y", colNames)]] <- data[data$subject == subjects[i], colNames[grep("_y", colNames)]] / isF_y
    }
    invisible(data)
}
