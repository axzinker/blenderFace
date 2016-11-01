#' Plot marker and quartile ellipse per condition
#' 
#' Plots the median and quartile ellipse per experimental condition
#' 
#' @param data Data frame containig the data to be plotted.
#' @param colNames Character vector naming the two columns with the x/y-Values
#'   to be plotted.
#' @param colNameCond Character vector with a single value containig the name 
#' of the stimulus type column of the data data-frame.
#' @param title Default is no title.
#' @param xlim Numeric vector of two elements defining the x-axis range of the 
#' plot. Per default the scaling is done automatically.
#' @param ylim Numeric vector of two elements defining the y-axis range of the 
#' plot. Per default the scaling is done automatically.
#' @param verbose Logical value. If TRUE, the function provides verbose 
#'   console output.
#'   
#' @return See vignette for example plots.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'  
#' @include plotrix
#'   
#' @examples
#' plotMmpCond(data = dataStdFCen, colNames = c("AU_12_L_x", "AU_12_L_y"), 
#'             colNameCond = "Stimulustype", title = "AU_12_L")
#'    
#' @export
plotMmpCond <- function(data, colNames, colNameCond, title = "", xlim = NA, ylim = NA, verbose = FALSE) {
    # Error handling
    if (!(is.data.frame(data))) {
        stop("Argument data does not contain a data frame!")
    }
    if (!(is.character(colNames)) | length(colNames) != 2) {
        stop("Argument colNames is missing, not of type character, or contains not two names (x/y columns)!")
    }
    if (!(is.character(colNameCond)) | length(colNameCond) != 1) {
        stop("Argument colNameCond is not of type character!")
    }
    if (!(is.character(title))) {
        stop("Argument title is not of type character!")
    }
    if (!is.na(xlim[1]) & (!(is.numeric(xlim)) | length(xlim) != 2)) {
      stop("Argument xlim is not numeric or not containing two elements!")
    }
    if (!is.na(ylim[1]) & (!(is.numeric(ylim)) | length(ylim) != 2)) {
      stop("Argument ylim is not numeric or not containing two elements!")
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    # Helper functions
    fcat <- function(...,newline=TRUE) {if (newline) cat(...,"\n") else cat(...); flush.console() }  # immediate console output
    
    # Hard coded constants
    ConstMin <- -0.005
    ConstMax <-  0.005
    
    tempData <- as.data.frame(cbind(data[colNames[1]],data[colNames[2]],data[colNameCond]))
    names(tempData) <- c("x","y","cond")
    
    # Extracting conditions
    cond <- unique(na.omit(unlist(tempData$cond)))
    cond <- cond[nchar(cond) > 0]
    
    # Compute mins and maxs for xlim/ylim of basic plot
    # This must be done for each stimulus condition for the addition of (medians + quantiles)
    # Preallocate minMaxData frame
    minMaxData <- as.data.frame(matrix(data = NA, nrow = length(cond), ncol = 5))
    names(minMaxData) <- c("minX","maxX","minY","maxY","cond")
    
    for (i in 1:length(cond)) {
      minMaxData$cond[i] <- cond[i]
      if (!is.infinite(min(tempData[tempData$cond == cond[i],"x"], na.rm = TRUE))) {
        minMaxData$minX[i] <- median(tempData[tempData$cond == cond[i],"x"], na.rm = TRUE) + as.numeric(quantile(tempData[tempData$cond == cond[i],"x"], .25, na.rm = TRUE))
      } else {
        minMaxData$minX[i] <- ConstMin
      }
      if (!is.infinite(max(tempData[tempData$cond == cond[i],"x"], na.rm = TRUE))) {
        minMaxData$maxX[i] <- median(tempData[tempData$cond == cond[i],"x"], na.rm = TRUE) + as.numeric(quantile(tempData[tempData$cond == cond[i],"x"], .75, na.rm = TRUE))
      } else {
        minMaxData$maxX[i] <- ConstMax
      }
      if (!is.infinite(min(tempData[tempData$cond == cond[i],"y"], na.rm = TRUE))) {
        minMaxData$minY[i] <- median(tempData[tempData$cond == cond[i],"y"], na.rm = TRUE) + as.numeric(quantile(tempData[tempData$cond == cond[i],"y"], .25, na.rm = TRUE))
      } else {
        minMaxData$minY[i] <- ConstMin
      }
      if (!is.infinite(max(tempData[tempData$cond == cond[i],"y"], na.rm = TRUE))) {
        minMaxData$maxY[i] <- median(tempData[tempData$cond == cond[i],"y"], na.rm = TRUE) + as.numeric(quantile(tempData[tempData$cond == cond[i],"y"], .75, na.rm = TRUE))
      } else {
        minMaxData$maxY[i] <- ConstMax
      }
    }
    # Overall min and max to achieve quadratic plots (no different scalings for x/y-axes; angles can be compared)
    min <- min(c(min(minMaxData$minX), min(minMaxData$minY)))
    max <- max(c(max(minMaxData$maxX), max(minMaxData$maxY)))
    
    # Make sure that 0 is contained in the min-max interval (0 is the origin of the movement -> use function centerCond first!)
    if(min > 0) {min <- 0}
    if(max < 0) {max <- 0}
    
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
    
    # Generate basic plot
    plot(c(minX,maxX),c(minY,maxY), type = "n", xlab = "Median movement x-Axis", ylab = "Median movement y-Axis", main = title)
    legend("topright", cex = 0.7, inset = 0, cond, col = 1:length(cond), pch = 19, horiz = FALSE)
    points(0, 0, pch = 19, col = 1)
    # Draw points, lines and ellipses per condition
    for (j in 1:length(cond)) {
        if (verbose) {
            fcat(paste("Plotting condition '", cond[j], "'", sep = ""))
        }
        medianX <- median(subset(tempData$x, subset = (tempData$cond == cond[j])), na.rm = TRUE)
        medianY <- median(subset(tempData$y, subset = (tempData$cond == cond[j])), na.rm = TRUE)
        quantX <- quantile(subset(tempData$x, subset = (tempData$cond == cond[j])), c(0.25, 0.75), na.rm = TRUE)
        quantY <- quantile(subset(tempData$y, subset = (tempData$cond == cond[j])), c(0.25, 0.75), na.rm = TRUE)
        lines(c(0, medianX), c(0, medianY), type = "l", col = j)
        points(medianX, medianY, pch = 19, col = j)
        plotrix::draw.ellipse(medianX, medianY, a = abs(quantX[2]), b = abs(quantY[2]), segment = c(0, 90), arc.only = FALSE, border = j, lty = 1, lwd = 1, deg = TRUE)
        plotrix::draw.ellipse(medianX, medianY, a = abs(quantX[1]), b = abs(quantY[2]), segment = c(90, 180), arc.only = FALSE, border = j, lty = 1, lwd = 1, deg = TRUE)
        plotrix::draw.ellipse(medianX, medianY, a = abs(quantX[1]), b = abs(quantY[1]), segment = c(180, 270), arc.only = FALSE, border = j, lty = 1, lwd = 1, deg = TRUE)
        plotrix::draw.ellipse(medianX, medianY, a = abs(quantX[2]), b = abs(quantY[1]), segment = c(270, 360), arc.only = FALSE, border = j, lty = 1, lwd = 1, deg = TRUE)
    }
}
