#' Computes distance and angle from marker columns
#' 
#' Computes the distance and the angle for a (mean, median) x/y marker 
#' point. The distance and the angle is computed from x = 0, y = 0 to the 
#' x/y - values given. Therefore, to get correct values for distance and angle
#' use the function \code{\link{centerCond}} before, to set the x/y-start 
#' values for each condition to x = 0, y = 0.
#' 
#' @param data Data frame containig the data for a subset, defined in colNames, 
#' for which the angle and the distance should be computed for.
#' @param colNames Character vector naming the two columns with the x/y-Values
#'   for which the angle and the distance should be computed.
#' @param colNameSubj Character vector with a single value containig the name 
#' of the subject column of the data data-frame.
#' @param rndDig Number of round digits. Default is 2.
#' @param verbose If TRUE, the function prints verbose output. Otherwise not. 
#' Default is FALSE.
#'
#' @import stats
#'    
#' @return Numeric matrix with median distance and angle.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' colNames <- c("AU_01_L_x", "AU_01_L_y", "AU_01_R_x", "AU_01_R_y", 
#'               "AU_02_L_x", "AU_02_L_y", "AU_02_R_x", "AU_02_R_y", 
#'               "AU_06_L_x", "AU_06_L_y", "AU_06_R_x", "AU_06_R_y", 
#'               "AU_08_x", "AU_08_y", 
#'               "AU_09_L_x", "AU_09_L_y", "AU_09_R_x", "AU_09_R_y", 
#'               "AU_10_L_x", "AU_10_L_y", "AU_10_R_x", "AU_10_R_y",  
#'               "AU_11_L_x", "AU_11_L_y", "AU_11_R_x", "AU_11_R_y",
#'               "AU_12_L_x", "AU_12_L_y", "AU_12_R_x", "AU_12_R_y", 
#'               "AU_16_x", "AU_16_y")
#' 
#' # Data preparation
#' data_Subj_happy <- subset(dataSmm, subset = dataSmm["Stimulustype"] == "posed_happy",
#'                           select = c("subject",colNames))
#' angleDist(data_Subj_happy, colNames = c("AU_12_L_x", "AU_12_L_y"),
#'           colNameSubj = "subject", rndDig = 3)
#' 
#' @export
angleDist <- function(data, colNames, colNameSubj, rndDig = 2, verbose = FALSE) {
  # Error handling
  if (!(is.data.frame(data))) {
    stop("Argument data does not contain a data frame!")
  }
  if (!(is.character(colNames)) | length(colNames) != 2) {
    stop("Argument colNames is missing, not of type character, or contains not two names (x/y columns)!")
  }
  if (!(is.character(colNameSubj)) | length(colNameSubj) != 1) {
    stop("Argument colNameSubj is not of type character!")
  }
  if (!(is.logical(verbose))) {
    stop("Argument verbose is not of type logical!")
  }
  
  # Helper functions
  fcat <- function(...,newline=TRUE) {if (newline) cat(...,"\n") else cat(...); flush.console() }  # immediate console output
  
  getDistance <- function(Point1, Point2) { # points must be with x/y-values
    # Computes distance between two points in a coordinate system (pytagoras). Its a two dimensional distance (not eukledian)
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
  
  subjects <- unique(data[[colNameSubj]])   # Determine number of subjects in data
  
  # Data preparation for functions
  # Preallocate temporary data frame
  tempData <- as.data.frame(matrix(data = NA, nrow = length(subjects), ncol = 3))
  names(tempData) <- c("subject","angle","distance")
  
  for(i in 1:length(subjects)) {
    if(verbose) {
      fcat(paste("Computing angle and distance for subject ", subjects[i], sep=""))
    }
    tempData[i,"subject"] <- subjects[i]
    tempData[i,"angle"] <- round(getAngle(median(as.numeric(unlist(subset(data, subset = (data[colNameSubj] == subjects[i]), select = colNames[1]))), na.rm = TRUE), median(as.numeric(unlist(subset(data, subset = (data[colNameSubj] == subjects[i]), select = colNames[2]))), na.rm = TRUE)), digits = rndDig)
    tempData[i,"distance"] <- round(getDistance(c(0,0), c(median(as.numeric(unlist(subset(data, subset = (data[colNameSubj] == subjects[i]), select = colNames[1]))), na.rm = TRUE), median(as.numeric(unlist(subset(data, subset = (data[colNameSubj] == subjects[i]), select = colNames[2]))), na.rm = TRUE))), digits = rndDig)
  }
  
  return(tempData)
}
