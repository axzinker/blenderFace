#' Plot potential outliers for specific frames 
#' 
#' The function computes the difference of the marker coordinate (x, y, or z) of 
#' frame \emph{t} with frame \emph{t + 1}. An outlier is identified by hitting 
#' a cut off definded by a multiplicative of the standard deviation (SD). The SD
#' is computed for the whole difference vector. The default cut off value is 
#' 3 SD. The function plots for each potential outlier the movemnent of the 
#' centered marker coordinate (x, y, or z), the deviation of the movement, the upper 
#' and lower cut off values and marks the frame with outlier. Use the function
#' ahead of code{\link{centerCond}} otherwise the centering may be identified
#' as an outlier.
#' 
#' @param data Data-frame containing the variables labeled in colNameFrames,
#' colNameData, and colNameCond.
#' @param colNameFrames Character vector containing the column name of the 
#' frames column of the data data-frame
#' @param colNameData Character vector containing the column name of the 
#' data which should be analyzed for outliers (e.g., AU_01_L_x)
#' @param colNameCond Character vector containing the column name of the
#' stimuli presented at each frame. Default is no colNameCond.
#' @param SDFactor Numeric vector of length 1. Factor the SD is multiplied with.
#' Default is 3.
#' @param title Default is no title.
#' @param verbose Logical value. If verbose is TRUE, the plots for each outlier
#' will be presented separately.
#'
#' @import utils
#'    
#' @return See vignette for example plots.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}, Rainer Alexandrowicz \email{rainer.alexandrowicz@aau.at}
#'   
#' @examples
#' \dontrun{
#' # fix me: meaningful example
#' }   
#' @export
plotOutliers <- function(data, colNameFrames, colNameData, colNameCond = "", SDFactor =  3, title = "", verbose = FALSE) {
  # Error handling
  if (!(nrow(data)) > 0 | !(is.data.frame(data))) {
    stop("Argument data is missing or of incorrect type!")
  }
  if (!(is.character(colNameFrames)) | length(colNameFrames) != 1) {
    stop("Argument colNameFrames is not of type character!")
  }
  if (!(is.character(colNameData)) | length(colNameData) != 1) {
    stop("Argument colNameData is not of type character!")
  }
  if (!(is.character(colNameCond)) | length(colNameCond) != 1) {
    stop("Argument colNameCond is not of type character!")
  }
  if (!is.na(SDFactor[1]) & (!(is.numeric(SDFactor)) | length(SDFactor) != 1)) {
    stop("Argument SDfactor is not numeric or containing more than one element!")
  }
  if (!(is.character(title))) {
    stop("Argument title is not of type character!")
  }
  if (!(is.logical(verbose))) {
    stop("Argument verbose is not of type logical!")
  }
  
  # Static data
  FramesOffset <- 1 # Frames for which the difference is computed (1 -> (t, t+1); 2 -> (t, t+2))
  FramesRange <- 50 # Range of frames to be plotted before and after each possible outlier
  PlotColor <- c("black","red","blue","darkgreen","darkgrey") # Colors for (1) data, (2) SD cut-off, (3) outlier, (4) t / t+n difference, (5) Stimulus
  CenterData <- TRUE # center raw data that it fits to the difference score on the plot
  
  # Helper functions
  fcat <- function(...,newline=TRUE) {if (newline) cat(...,"\n") else cat(...); flush.console() }  # immediate console output
  
  # If not data is given, break funktion at this point
  if(unique(is.na(data[colNameData])) == TRUE) {
    fcat(paste0("No data for column ",colNameData))
    return()
  }
    
  # Build temporary data frame used for outlier detection
  tempData <- as.data.frame(cbind(data[colNameFrames], data[colNameData], data[colNameCond]))
  names(tempData) <- c("frames", "data", "cond")
  
  # Compute difference of movement t - (t - FramesOffset)
  # => compute difference of movement with the next frame
  diff <- tempData$data - c(rep(NA,FramesOffset),tempData$data[1:(length(tempData$data)-FramesOffset)])
  # diff lags FramesOffset frames behine, moving it to its original position
  diff <- c(diff[-FramesOffset],rep(NA,FramesOffset))
  tempData <- cbind(tempData,diff)
  
  # Compute Standard deviation
  tempSD <- sd(tempData$diff, na.rm = TRUE)
  # Multiply SD with SDFactor
  tempSD <- tempSD * SDFactor
  
  # Compute possible outliers
  outliers <- (abs(tempData$diff) > tempSD)
  tempData <- cbind(tempData, outliers)
  
  # Get frames with possible outliers
  outlierFrames <- subset(tempData$frames, subset = (tempData$outliers == TRUE))
  
  for (i in 1:length(outlierFrames)) {
    startFrame <-  outlierFrames[i] - FramesRange
    stopFrame <- outlierFrames[i] + FramesRange
    plotData <- subset(tempData, subset = ((tempData$frame > startFrame) & (tempData$frame < stopFrame)))
    
    if(CenterData) {
      # Subtract value of first frame from remaining frame window  
      plotData$data <- plotData$data - plotData$data[1]                         
      } else {
      plotData$data <- tempData$data
    }
    
    plot(plotData$frames, plotData$data, type = "l", xlim = c(startFrame,stopFrame), xlab = "Frames", ylab = "Deviation", main = title, sub = paste("Possible outlier at Frame ",outlierFrames[i],sep=""))
    abline(h = tempSD, col="red")
    abline(h = -tempSD, col="red")
    abline(v = (outlierFrames[i]), col="blue")
    points(tempData$frames, tempData$diff, col="darkgreen", type="l")
    
    # If present, draw stimuli episodes as labeled rectangles 
    if(colNameCond != "") {
      
      # find unique stimulus conditions within the plotted section
      tempPlotSection <- subset(tempData, subset = ((tempData$frames > startFrame) & (tempData$frames < stopFrame)))
      colCond <- na.omit(unique(tempPlotSection$cond))[1]
      
      for(j in 1:length(colCond)) {
        # compute min and max frames for the actual condition
        minCond <- min(subset(tempPlotSection$frames, subset = (tempPlotSection$cond == colCond[j])))
        maxCond <- max(subset(tempPlotSection$frames, subset = (tempPlotSection$cond == colCond[j])))
        maxY <- max(tempData$data,rm.na=TRUE)
        minY <- min(tempData$data,rm.na=TRUE)
        # plot the actual condition
        rect(xleft = minCond, ybottom = minY, xright = maxCond, ytop = maxY, lty = 3, lwd = .5, col = "transparent")
        text(x = (maxCond - minCond)/2 + minCond, y = (maxY - minY)/3 + minY, labels = colCond[j], col = "gray60")
      } 
      
      # make legend when condition is available
      legend("bottomright", c("Cen. data", "SDFactor cut-off", "Outlier","Difference","Condition"), col = PlotColor, lty = 1)  
    } else  {
      # make legend without condition
      legend("bottomright", c("Cen. data", "SDFactor cut-off", "Outlier","Difference"), col = PlotColor[-(length(PlotColor))], lty = 1)
    }
    
    
    if(verbose) {
      fcat("Next plot? (Press return to coninue, or 'c' to cancel)")
      if (tolower(readline(prompt = "? ")) == "c") {
        stop("Aborted due to user request.")
      }
    }
  }
}
