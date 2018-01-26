#' Plots median movement of a marker per subject
#' 
#' Plots median movement of a marker per subject and labels it with subject 
#' number. This function may be used to identify individual outliers. To achieve 
#' meaninful plots, use the function \code{\link{centerCond}} first.
#' 
#' @param data Data frame containig the data columns to be plotted (among other
#'   columns).
#' @param colNames Character vector naming the two columns for with the x/y-Values
#'   should be plotted.
#' @param colNameSubj Character vector with a single value containig the name 
#'   of the subject column of the data data-frame.
#' @param title Character string containing the title. Default is no title.
#' @param xlim Numeric vector of two elements defining the x-axis range of the 
#'   plot. Default is empty (NA), to compute the min and max values to achieve 
#'   a quadratic plot.
#' @param ylim Numeric vector of two elements defining the y-axis range of the 
#'   plot. Default is empty (NA), to compute the min and max values to achieve 
#'   a quadratic plot.
#' @param verbose Logical value. If TRUE, the function provides verbose 
#'   console output.
#'
#' @import utils
#' @import calibrate
#'   
#' @return See vignette for example plots.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' colNames <- c("A7_x",  "A7_y",  "A8_x",  "A8_y",  
#'               "BL2_x", "BL2_y", "BL4_x", "BL4_y",  
#'               "BL5_x", "BL5_y", "BL7_x", "BL7_y",        
#'               "BR2_x", "BR2_y", "BR4_x", "BR4_y",  
#'               "BR5_x", "BR5_y", "BR7_x", "BR7_y",  
#'               "CL4_x", "CL4_y", "CL7_x", "CL7_y",        
#'               "CR4_x", "CR4_y", "CR7_x", "CR7_y",  
#'               "DL2_x", "DL2_y", "DR2_x", "DR2_y")
#'               
#' # Select data for plotting (selecting stimulus type and omit z-axis)
#' data_Subj_happy <- subset(dataStdFCen, subset = (dataStdFCen$Stimulustype == "posed_happy"), 
#'                           select = c("subject",colNames))
#' 
#' plotIndmm(data = data_Subj_happy, colNames = c("CL7_x", "CL7_y"), 
#'           colNameSubj = "subject", title = "Posed Happy CL7")
#' @export
plotIndmm <- function(data, colNames, colNameSubj, title = "", xlim = NA,
                      ylim = NA, verbose = FALSE) {
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
  if (!(is.character(title))) {
    stop("Argument title is not of type character!")
  }
  if (!is.na(xlim[1]) & (!(is.numeric(xlim)) | length(xlim) != 2)) {
    stop("Argument xlim is not numeric or not containing two elements")
  }
  if (!is.na(ylim[1]) & (!(is.numeric(ylim)) | length(ylim) != 2)) {
    stop("Argument ylim is not numeric or not containing two elements")
  }
  if (!(is.logical(verbose))) {
    stop("Argument verbose is not of type logical!")
  }
  
  # Helper functions
  fcat <- function(...,newline=TRUE) {if (newline) cat(...,"\n") else cat(...); flush.console() }  # immediate console output
  
  # Hard coded constants
  ConstMin <- -0.005
  ConstMax <-  0.005
  
  subjects <- unique(data[[colNameSubj]])   # Determine number of subjects in data
  
  # x-values: data[colNames[1]]
  # y-values: data[colNames[2]]
  
  # Compute medians for all subjects and store them temporarily
  # Preallocate temporary data frame
  tempData <- as.data.frame(matrix(data = NA, nrow = length(subjects), ncol = 3))
  names(tempData) <- c("subject","x","y")
  
  for (i in 1:length(subjects)) {
    if (verbose) {
      fcat(paste("Compute median for Subject ", subjects[i], sep = ""))
    }
    tempData[i,"subject"] <- subjects[i]
    tempData[i,"x"] <- median(as.numeric(unlist(subset(data[colNames[1]], subset = (data[colNameSubj] == subjects[i])))), na.rm = TRUE)
    tempData[i,"y"] <- median(as.numeric(unlist(subset(data[colNames[2]], subset = (data[colNameSubj] == subjects[i])))), na.rm = TRUE)
  }
  
  # Prepare data for basic plot
  if (!is.infinite(min(tempData[,"x"], na.rm = TRUE))) {
    minX <- min(tempData[,"x"], na.rm = TRUE)
  } else {
    minX <- ConstMin
  }
  if (!is.infinite(max(tempData[,"x"], na.rm = TRUE))) {
    maxX <- max(tempData[,"x"], na.rm = TRUE)
  } else {
    maxX <- ConstMax
  }
  if (!is.infinite(min(tempData[,"y"], na.rm = TRUE))) {
    minY <- min(tempData[,"y"], na.rm = TRUE)
  } else {
    minY <- ConstMin
  }
  if (!is.infinite(max(tempData[,"y"], na.rm = TRUE))) {
    maxY <- max(tempData[,"y"], na.rm = TRUE)
  } else {
    maxY <- ConstMax
  }
  
  # Overall min and max to achieve quadratic plots (no different scalings in presentation!)
  min <- min(c(minX, minY))
  max <- max(c(maxX, maxY))
  
  # Make sure that 0 is contained in the min-max interval (0 is the origin of the movement -> use function centerCond first!)
  if (min > 0) {min <- 0}
  if (max < 0) {max <- 0}
  
  # if user-xlim or user-ylim are given, overwrite computed values
  if (!unique(is.na(xlim))) { # TRUE, if user presets given
    minX <- xlim[1]
    maxX <- xlim[2]
  } else {
    minX <- min
    maxX <- max
  }
  if (!unique(is.na(ylim))) { # TRUE, if user presets given
    minY <- ylim[1]
    maxY <- ylim[2]
  } else {
    minY <- min
    maxY <- max
  }
  
  # Make basic plot
  plot(c(minX,maxX),c(minY,maxY),type = "n", main = title, xlab = "Median movement x-axis", ylab = "Median movement y-axis")
  points(0,0,pch = 19,col = 1)
  
  # Plot individual values
  for (i in 1:length(subjects)) {
    if (verbose) {
      fcat(paste("Plot Subject ",subjects[i],sep = ""))
    }
    
    points(tempData[i,"x"], tempData[i,"y"], pch = 19, col = 1)
    lines(c(0,tempData[i,"x"]),c(0,tempData[i,"y"]),type = "l",col = 1)
    if (!is.na(tempData[i,"x"])) {textxy(tempData[i,"x"],tempData[i,"y"],tempData[i,"subject"],cex = 1)}
  }
}
