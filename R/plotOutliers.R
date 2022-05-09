#' Plot potential outliers for specific frames 
#' 
#' Because in this case the SD is inappropriate for the purpose of outlier detection
#' (e.g., if there is rarely movement, even very small movements / noise are 
#' detected as outliers), a theoretical approach is used: The fastest human 
#' movement is the eyeblink, which takes about 300 - 400 ms (Robert A.Moses 
#' (Ed.), (1981). Adler's Physiology of the eye clinical application. Mosby, 
#' Chapter 1, p. 1-15). The cornea of the eye has a diameter of 11.5 mm. The 
#' speed of the eye-blink movement can be therefore 
#' (11.5mm / 150 ms) = 0.0766 mm/ms. Based on the framerate (fps), the maximal 
#' plausible movement from one frame to the next is computed, and taken as a 
#' cut off value. For example, a video clip recorded at 30fps contains an 
#' image taken every 33ms. Therefore the maximal distance from one frame to 
#' the next is 0.0766 mm/ms * 33 ms = 2.53 mm. Every larger 
#' distance is marked as an outlier.
#' The function computes the 2D distance (Pytagoras) of a marker coordinates
#' pair (x and y) of frame \emph{t} and frame \emph{t + 1}. Per default, any 
#' distance larger than 2.53 mm is marked as a potential outlier. The function 
#' plots for each potential outlier the movement of the centered marker 
#' coordinates (x and y), the distance from \emph{t} to \emph{t + 1}, the upper 
#' cut off value, and marks the frame with the outlier. This function
#' needs data scaled to mm because the cut off value is scaled in mm. Therefore,
#' use the function {\link{bu2mm}} beforehand, otherwise the outlier detection
#' is not meaningful.
#' 
#' @param data Data-frame containing the variables labeled in colNameFrames,
#' colNameData, and colNameCond.
#' @param colNameFrames Character vector containing the column name of the 
#' frames column of the data data-frame
#' @param colNameData Character vector containing the column names for x and y 
#' values of the data which should be analyzed for outliers (e.g., BL2_x, BL2_y)
#' @param colNameCond Character vector containing the column name of the
#' stimuli presented at each frame. Default is no colNameCond.
#' @param maxM Numeric vector of length 1. Cut off value (maximal allwed 
#' movement) in mm. Movments larger than maxM are marked as outliers. Optional 
#' value, default is 2.2 mm.
#' @param title Default are the column names.
#' @param savePlots Set to TRUE if outlier plots should be saved as a pdf. As a 
#' filename the title is used. Default is FALSE.
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
plotOutliers <- function(data, colNameFrames, colNameData, colNameCond = "", maxM = 2.53, title = colNameData, savePlots = FALSE, verbose = FALSE) {
  # Axel: fix me: footage error handling (e.g., detect artefacts because of wrong/changing framerates)
  # Axel: fix me: write separate plot helper function (at the moment the code is duplicated)
  if (!(nrow(data)) > 0 | !(is.data.frame(data))) {
    stop("Argument data is missing or of incorrect type!")
  }
  if (!(is.character(colNameFrames)) | length(colNameFrames) != 1) {
    stop("Argument colNameFrames is not of type character!")
  }
  if (!(is.character(colNameData)) | length(colNameData) != 2) {
    stop("Argument colNameData is missing, not of type character, or does not contain two names (x/y columns)!")
  }
  if (!(is.character(colNameCond)) | length(colNameCond) != 1) {
    stop("Argument colNameCond is not of type character!")
  }
  if (!is.na(maxM[1]) & (!(is.numeric(maxM)) | length(maxM) != 1)) {
    stop("Argument maxM is not numeric or containing more than one element!")
  }
  if (!(is.character(title))) {
    stop("Argument title is not of type character!")
  }
  if (!(is.logical(verbose))) {
    stop("Argument verbose is not of type logical!")
  }

  # Static data
  FramesOffset <- 1 # Frame offset for which the difference is computed (1 -> (t, t+1); 2 -> (t, t+2))
  FramesRange <- 50 # Range of frames to be plotted before and after each possible outlier
  PlotColor <- c("black","brown","red","blue","darkgreen","darkgrey") # Colors for (1,2) data, (2) SD cut-off, (3) outlier, (4) t / t+n difference, (5) Stimulus
  CenterData <- TRUE # center raw data that it fits to the difference score on the plot
  
  # Helper functions
  fcat <- function(...,newline=TRUE) {if (newline) cat(...,"\n") else cat(...); flush.console() }  # immediate console output
  
  getDistance <- function(Point1, Point2) { # points must be with x/y-values (vector or matrix/df columns)
    # Computes distance between two points in a coordinate system (pytagoras). Its a two dimensional distance (not eukledian)
    # same function as in angleDist.R
    # unify input parameters (allowed is vector (x,y value per parameter) or dataframe/matrix (col_x, col_y per parameter))
    if (is.null(ncol(Point1))) {
      Point1 <- t(as.matrix(Point1))
      Point2 <- t(as.matrix(Point2))
    } else {
      Point1 <- as.matrix(Point1)
      Point2 <- as.matrix(Point2)
    }
    retData <- sqrt((Point2[,1] - Point1[,1]) ^ 2 + (Point2[,2] - Point1[,2]) ^ 2)
    return(retData)
  }
  
  # If one of the columns does not contain (enough) data but only NAs, break funktion at this point
  if (sum(is.na(data[colNameData][1])) == nrow(data[colNameData][1]) |
      sum(is.na(data[colNameData][1])) == nrow(data[colNameData][1]) - 1 |
      sum(is.na(data[colNameData][2])) == nrow(data[colNameData][2]) |
      sum(is.na(data[colNameData][2])) == nrow(data[colNameData][2]) - 1) {
    fcat(paste("No data (colNameData), only NAs at least in one of the columns: ", colNameData[1],", ",colNameData[2], sep = ""))
    return()
  }
  
  # Build temporary data frame used for outlier detection
  if (colNameCond == ""){
    tempData <- as.data.frame(cbind(data[colNameFrames], data[colNameData][1], data[colNameData][2]))
    names(tempData) <- c("frames", "data_x_t2", "data_y_t2")
  } else {
    tempData <- as.data.frame(cbind(data[colNameFrames], data[colNameData][1], data[colNameData][2], data[colNameCond]))
    names(tempData) <- c("frames", "data_x_t2", "data_y_t2", "cond")
  }
  rm(data)
  
  # Compute difference of movement t - (t - FramesOffset)
  # => compute difference of movement (pytagoras) with the next frame
  # Prepare data
  offsetMatrix <- as.data.frame(matrix(rep(NA,FramesOffset * 2), ncol = 2))
  names(offsetMatrix) <- c("data_x_t1", "data_y_t1")
  tempData_t2 <- tempData[c("data_x_t2","data_y_t2")]
  names(tempData_t2) <- c("data_x_t1", "data_y_t1")
  tempData_t1 <- rbind(offsetMatrix, tempData_t2)
  tempData_t1 <- tempData_t1[-((nrow(tempData_t1) - FramesOffset + 1):nrow(tempData_t1)),]
  tempData <- cbind(tempData, tempData_t1)
  # Delete offset columns at the beginning of the matrix because there is no difference to compute
  tempData <- tempData[-(1:FramesOffset),]
  rm(list = c("tempData_t1","tempData_t2","offsetMatrix"))
  # Compute pytagorean distance
  dist <- getDistance(tempData[c("data_x_t1","data_y_t1")], tempData[c("data_x_t2","data_y_t2")])
  tempData <- cbind(tempData,dist = dist)
  rm(dist)
  
  # Compute possible outliers
  outliers <- (abs(tempData$dist) > maxM)
  tempData <- cbind(tempData, outliers)
  
  # correct for outlier-artefacts by data subsets / footage cuts (= non consecutive frame numbers)
  # get rowlinenumber containing outliers
  outlierLinNums <- which(tempData$outliers == TRUE)
  # test if line numbers are consecutive
  if ((length(outlierLinNums) >= 1) & (outlierLinNums[1] > 1)) {
    for (i in 1:length(outlierLinNums)) {
      if (!(tempData[outlierLinNums[i] - 1,"frames"] + 1 == tempData[outlierLinNums[i],"frames"])) {
        if (verbose) {
          fcat(paste("Non-consecutive frame numbers detected and corrected (frame ",tempData[outlierLinNums[i] - 1,"frames"]," followed by frame ",tempData[outlierLinNums[i],"frames"]," (wrongly identified as outlier))!", sep = ""))
        }
        tempData[outlierLinNums[i],"outliers"] <- FALSE
      }
    }
  }
  
  # Get frames with possible outliers
  outlierFrames <- subset(tempData$frames, subset = (tempData$outliers == TRUE))
  
  # if there are outliers, do the following:
  if (sum(tempData$outliers, na.rm = TRUE) >= 1) {
    fcat(paste(sum(tempData$outliers, na.rm = TRUE)," possible outlier(s) found:",sep = ""))
    print.table(cbind(Dist = subset(tempData$dist, subset = (tempData$outliers == TRUE)), Frames = outlierFrames))
    
    if (verbose) {# verbose -> make the plots with user interaction after each plot
      fcat(paste("Plot each outlier? ('y', 'n', or 'c' to cancel)", sep = ""))
      userinput <- ""
      while (!((userinput == "y") | (userinput == "n") | (userinput == "c")) ) {
        userinput <- tolower(readline(prompt = "(y,n,c)? "))  
      }
      
      if (userinput == "y") {
        
        for (i in 1:length(outlierFrames)) {
          startFrame <-  outlierFrames[i] - FramesRange
          stopFrame <- outlierFrames[i] + FramesRange
          plotData <- subset(tempData, subset = ((tempData$frames >= startFrame) & (tempData$frames <= stopFrame)))
          
          if (CenterData) {
            # Subtract value of first frame from remaining frame window  
            plotData$data_x_t1 <- plotData$data_x_t1 - mean(plotData$data_x_t1, na.rm = TRUE)
            plotData$data_x_t2 <- plotData$data_x_t2 - mean(plotData$data_x_t2, na.rm = TRUE)
            plotData$data_y_t1 <- plotData$data_y_t1 - mean(plotData$data_y_t1, na.rm = TRUE)
            plotData$data_y_t2 <- plotData$data_y_t2 - mean(plotData$data_y_t2, na.rm = TRUE)
          }
          
          plotYlimMin <- min(c(plotData$data_x_t1, plotData$data_y_t1, plotData$data_x_t2, plotData$data_y_t2, plotData$dist), na.rm = TRUE)
          plotYlimMax <- max(c(plotData$data_x_t1, plotData$data_y_t1, plotData$data_x_t2, plotData$data_y_t2, plotData$dist), na.rm = TRUE)
          
          plot(plotData$frames, plotData$data_x_t1, type = "l", xlim = c(startFrame,stopFrame), ylim = c(plotYlimMin,plotYlimMax),
               xlab = "Frames", ylab = "Movement", main = title, 
               sub = paste("Possible outlier at frame ",outlierFrames[i]," (Dist.: ",round(plotData$dist[plotData$frames == outlierFrames[i]],digits = 2),")",sep = ""))
          points(plotData$frames, plotData$data_y_t1, col = "brown", type = "l")
          abline(h = maxM, col = "red")
          abline(v = (outlierFrames[i]), col = "blue")
          points(plotData$frames, plotData$dist, col = "darkgreen", type = "l")
          
          # If present, draw stimuli episodes as labeled rectangles 
          if (colNameCond != "") {
            # find unique stimulus conditions within the plotted section
            colCond <- na.omit(unique(plotData$cond))[1]
            
            for (j in 1:length(colCond)) {
              # compute min and max frames for the actual condition
              minCond <- min(subset(plotData$frames, subset = (plotData$cond == colCond[j])))
              maxCond <- max(subset(plotData$frames, subset = (plotData$cond == colCond[j])))
              # plot the actual condition
              rect(xleft = minCond, ybottom = plotYlimMin, xright = maxCond, ytop = plotYlimMax, lty = 3, lwd = .5, col = "transparent")
              text(x = (maxCond - minCond)/2 + minCond, y = (plotYlimMax - plotYlimMin)/3 + plotYlimMin, labels = colCond[j], col = "gray60")
            } 
            
            # make legend when condition is available
            legend("bottomright", c("Cen. data x", "Cen. data y", "Cut-off", "Outlier",paste("t+",FramesOffset," Distance", sep = ""),"Condition"), col = PlotColor, lty = 1)  
          } else {
            # make legend without condition
            legend("bottomright", c("Cen. data ", "Cen. data y", "Cut-off", "Outlier",paste("t+",FramesOffset," Distance",sep = "")), col = PlotColor[-(length(PlotColor))], lty = 1)
          }
          
          if (i < length(outlierFrames)) {
            fcat("Next plot? (Press return to coninue, or 'c' to cancel)")
            if (tolower(readline(prompt = "? ")) == "c") {
              stop("Aborted due to user request.")
            }
          }
        }
      } # end if 'y'
      
      if (userinput == "n") {
        fcat("Skipped plots due to user request.")
      }
      
      if (userinput == "c") {
        stop("Script aborted due to user request.")
      }
    } else {# not verbose -> make the plots without asking
      
      for (i in 1:length(outlierFrames)) {
        startFrame <-  outlierFrames[i] - FramesRange
        stopFrame <- outlierFrames[i] + FramesRange
        plotData <- subset(tempData, subset = ((tempData$frames >= startFrame) & (tempData$frames <= stopFrame)))
        
        if (CenterData) {
          # Subtract value of first frame from remaining frame window  
          plotData$data_x_t1 <- plotData$data_x_t1 - mean(plotData$data_x_t1, na.rm = TRUE)
          plotData$data_x_t2 <- plotData$data_x_t2 - mean(plotData$data_x_t2, na.rm = TRUE)
          plotData$data_y_t1 <- plotData$data_y_t1 - mean(plotData$data_y_t1, na.rm = TRUE)
          plotData$data_y_t2 <- plotData$data_y_t2 - mean(plotData$data_y_t2, na.rm = TRUE)
        }
        
        if (savePlots) {
          pdf(file = paste0(title,"_Frame_",outlierFrames[i],".pdf"), onefile = TRUE)
        }
        
        plotYlimMin <- min(c(plotData$data_x_t1, plotData$data_y_t1, plotData$data_x_t2, plotData$data_y_t2, plotData$dist), na.rm = TRUE)
        plotYlimMax <- max(c(plotData$data_x_t1, plotData$data_y_t1, plotData$data_x_t2, plotData$data_y_t2, plotData$dist), na.rm = TRUE)
        
        plot(plotData$frames, plotData$data_x_t1, type = "l", xlim = c(startFrame,stopFrame), ylim = c(plotYlimMin,plotYlimMax),
             xlab = "Frames", ylab = "Movement", main = title, 
             sub = paste("Possible outlier at frame ",outlierFrames[i]," (Dist.: ",round(plotData$dist[plotData$frames == outlierFrames[i]],digits = 2),")",sep = ""))
        points(plotData$frames, plotData$data_y_t1, col = "brown", type = "l")
        abline(h = maxM, col = "red")
        abline(v = (outlierFrames[i]), col = "blue")
        points(plotData$frames, plotData$dist, col = "darkgreen", type = "l")
        
        # If present, draw stimuli episodes as labeled rectangles 
        if (colNameCond != "") {
          # find unique stimulus conditions within the plotted section
          colCond <- na.omit(unique(plotData$cond))[1]
          
          for (j in 1:length(colCond)) {
            # compute min and max frames for the actual condition
            minCond <- min(subset(plotData$frames, subset = (plotData$cond == colCond[j])))
            maxCond <- max(subset(plotData$frames, subset = (plotData$cond == colCond[j])))
            # plot the actual condition
            rect(xleft = minCond, ybottom = plotYlimMin, xright = maxCond, ytop = plotYlimMax, lty = 3, lwd = .5, col = "transparent")
            text(x = (maxCond - minCond)/2 + minCond, y = (plotYlimMax - plotYlimMin)/3 + plotYlimMin, labels = colCond[j], col = "gray60")
          } 
          
          # make legend when condition is available
          legend("bottomright", c("Cen. data x", "Cen. data y", "Cut-off", "Outlier",paste("t+",FramesOffset," Distance", sep = ""),"Condition"), col = PlotColor, lty = 1)  
        } else {
          # make legend without condition
          legend("bottomright", c("Cen. data x", "Cen. data y", "Cut-off", "Outlier",paste("t+",FramesOffset," Distance",sep = "")), col = PlotColor[-(length(PlotColor))], lty = 1)
        }
        if (savePlots) {
          dev.off()
        }
      }
      
    }
  } else {# if there are no outliers, do the following:
    fcat(paste("No outliers found. Maximal movement is ",round(max(tempData$dist, na.rm = TRUE), digits = 2)," mm at Frame ",subset(tempData$frames,subset = (tempData$dist == max(tempData$dist, na.rm = TRUE))),".", sep = ""))
  }
}
