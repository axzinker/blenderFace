#' Scale head proportions
#' 
#' The heads of the participants are of different sizes. This function rescales
#' the x/y values according to a standardized face. The standardized face is of 
#' height 1 and width 1, while the pupil-pupil distance is defined as 1/3 and the 
#' left-pupil -- left-mouth-corner distance is defined as 1/3. Accordingly, for resizing
#' the x-axis, the distance between the pupils of the subject is used. For resizing the y-axis,
#' the distance between the left pupil and the left mouthcorner is used. 
#' Optionally, the distance between the mouthcorners for the x-axis, and the distance between
#' the right pupil and the right mouthcorner for the y-axis may be provided in addition.
#' If provided, a mean of both measures is computed. Be aware that if you previously rescaled 
#' the data into millimeter by the function \code{\link{bu2mm}}, you also have to
#' rescale the scaling parameters e.g., the pupil-pupil distance into millimeter first
#' and use the millimeter values as scaling parameters!
#' 
#' @param data Data frame containig the columns which should be rescaled.
#'   Remaining colums will be returned unchanged (e.g., the z-values because 
#'   no information for rescaling is available).
#' @param colNames Character vector with names of the columns which should be
#'   rescaled. To differ between x-, and y-axis scaling, the variable names containing horizontal movement (left-right) should end with '_x',
#'   whereas variable names containing vertical movement (up-down) should end with '_y'.
#' @param colNameSubj Character vector with a single value containig the name 
#'   of the subject column of the data data-frame.
#' @param pupilDist Numeric vector of measurement of the distance from left to right pupil for each subject,
#'   measured in blender units. This vector is used to rescale the x-axis.
#' @param leftPMDist Numeric vector of measuremet of the distance from left pupil to left
#'   mouthcorner for each subject, measured in blender units. This vector is used to rescale the y-axis.
#' @param mouthcornerDist Optional numeric vector of measurement of the distance from left to right
#'   mouthcorner for each subject measured in blender units. If provided, a mean 
#'   of \code{pupilDist} and \code{mouthcornerDist} is used for rescaling the x-axis.
#' @param rightPMDist Optional numeric vector of measurement of the distance from right pupil to right
#'   mouthcorner for each subject, measured in blender units. If provided, a mean 
#'   of \code{leftPMDist} and \code{rightPMDist} is used for rescaling the y-axis.
#' @param verbose Logical value. If TRUE, the function provides verbose console output.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}, 
#' Rainer Alexandrowicz \email{rainer.alexandrowicz@aau.at}
#'   
#' @examples
#' # Load the file "Blender_Scalingdata.csv"
#' scaledata <- read.csv(system.file("extdata", "Blender_Scalingdata.csv", package = "blenderFace"), header = TRUE, sep =",")
#' # Make sure to have the data sorted by subjects
#' scaledata <- scaledata[with(scaledata, order(scaledata$subject)), ]
#'
#' # Determin the dataframe columns which should be scaled:
#' names(rawdata)
#' # -> Frame, Stimulustype, subject and z-axis values should not be scaled -> removed for variable colNames
#' colNames <- c("AU_01_L_x", "AU_01_L_y", "AU_01_R_x", "AU_01_R_y", "AU_02_L_x", "AU_02_L_y", "AU_02_R_x", "AU_02_R_y", 
#' "AU_06_L_x", "AU_06_L_y", "AU_06_R_x", "AU_06_R_y", "AU_08_x", "AU_08_y", 
#' "AU_09_L_x", "AU_09_L_y", "AU_09_R_x", "AU_09_R_y", "AU_10_L_x", "AU_10_L_y", "AU_10_R_x", "AU_10_R_y",  
#' "AU_12_L_x", "AU_12_L_y", "AU_12_R_x", "AU_12_R_y", "AU_16_x", "AU_16_y")
#' 
#' # To not overwrite existing data, use a new data frame (dataStdF means data of standaradized faces)
#' dataStdF <- face2stdFace(data = rawdata, colNames = colNames, colNameSubj = "subject", pupilDist = scaledata$PupilPupilDistance, leftPMDist = scaledata$LeftPupilLeftMouthcornerDistance)
#' @return Data frame with columns rescaled to a standard face.
#'   
#' @export
face2stdFace <- function(data, colNames, colNameSubj, pupilDist, leftPMDist, mouthcornerDist = NA, rightPMDist = NA, verbose = FALSE) {
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
            fcat(paste("Perform scaling to standardized face for subject ", i, " of ", length(subjects), ".", sep = ""))
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
