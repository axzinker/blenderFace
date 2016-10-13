#' Plot x-, y-axis marker movement per frame
#' 
#' Plots separate lines for the x,y marker values (y-axis) per frame (x-axis).
#' 
#' @param frames numeric vector containing the frames for which x/y-values
#'   should be plotted
#' @param xMarker numeric vector with x values (same length as frames vector)
#' @param yMarker numeric vector with y values (same length as frames vector)
#' @param stimF character vector containing the stimuli presented at each frame (same length as frames vector). Stimulus labels must be unique. Optional. Default is NA.
#' @param center if TRUE, center x/y values by setting the x/y values to 0 at
#'   the first (appropriate, e.g. non NA frame) frame. Default = TRUE
#' @param color character vector of two values to define color for x/y values of
#'   the Marker. Default is c('red','orange').
#' @param title Default is no title.
#' @param overplot If TRUE, only the x/y lines, but no base graph is plotted.
#'   Default is FALSE.
#'   
#' @return See vignette for example plots.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}, Rainer Alexandrowicz \email{rainer.alexandrowicz@aau.at}
#'   
#' @examples
#' \dontrun{plotXYmmpf()}
#'   
#' @export
plotXYmmpf <- function(frames, xMarker, yMarker, stimF = "", center = TRUE, color = c("red", "orange"), title = "", overplot = FALSE) {
    # Error handling
    if (!(length(frames) > 0) | !(is.numeric(frames))) {
        stop("Argument frames missing or of incorrect type!")
    }
    if (!(is.numeric(xMarker)) | (length(xMarker) != length(frames))) {
        stop("Argument xMarker is not numeric or not of equal length as frames vector!")
    }
    if (!(is.numeric(yMarker)) | (length(yMarker) != length(frames))) {
        stop("Argument yMarker is not numeric or not of equal length as frames vector!")
    }
    if (length(stimF) > 1) {
      if (!(is.character(stimF)) | (length(stimF) != length(frames))) {
        stop("Argument stimF is not of type character or not of equal length as frames vector!")
      }
    }
    if (!(is.logical(center))) {
        stop("Argument center is not of type logical!")
    }
    if ((length(color) != 2) | !(is.character(color[1])) | !(is.character(color[2]))) {
        stop("Argument color is not of lenght 2 or of incorrect type!")
    }
    if (!(is.character(title))) {
        stop("Argument title is not of type character!")
    }
    if (!(is.logical(overplot))) {
        stop("Argument overplot is not of type logical!")
    }
    
    # Helper functions
    
    # Find first valid frame (which is not NA)
    minValidFrameValue <- function(frames, x) {
      # frames: vector with frames x: vector with variable
      
      # Step 1: build data frame
      tempData <- data.frame(frames, x)
      # Step 2: remove NAs
      tempData <- tempData[!is.na(tempData$x), ]
      # Step 3: select minimal framenumber (= order ascending by frames)
      tempData <- tempData[order(tempData$frames), ]
      # Step 4: select value of col with the lowest Framenumber (that is the first line, if data frame is ordered by frames)
      tempData <- tempData$x[1]
      return(tempData)
    }
    
    # Build data frame from separate parameter data to facilitate data management
    if (length(stimF) > 1) {
      data <- data.frame(frames, xMarker, yMarker, stimF)
    } else {
      data <- data.frame(frames, xMarker, yMarker)
    }
    rm("frames", "xMarker", "yMarker")
    
    if (center) {
      data$xMarker <- data$xMarker - minValidFrameValue(data$frames, data$xMarker)
      data$yMarker <- data$yMarker - minValidFrameValue(data$frames, data$yMarker)
      ylab <- "Centered deviation"
    } else {
      ylab <- "Deviation"
    }
    
    # Prepare data for basic plot
    if (!is.infinite(min(data$xMarker, na.rm = TRUE))) {
      minX <- min(data$xMarker, na.rm = TRUE)
    } else {
      minX <- -0.005
    }
    if (!is.infinite(max(data$xMarker, na.rm = TRUE))) {
      maxX <- max(data$xMarker, na.rm = TRUE)
    } else {
      maxX <- 0.005
    }
    if (!is.infinite(min(data$yMarker, na.rm = TRUE))) {
      minY <- min(data$yMarker, na.rm = TRUE)
    } else {
      minY <- -0.005
    }
    if (!is.infinite(max(data$yMarker, na.rm = TRUE))) {
      maxY <- max(data$yMarker, na.rm = TRUE)
    } else {
      maxY <- 0.005
    }

    min <- min(c(minX, minY))
    max <- max(c(maxX, maxY))
    
    # Do basic plot
    if (!overplot) {
        plot(1, type = "n", xlim = c(data$frame[1], data$frame[length(data$frame)]), ylim = c(min, max), main = title, xlab = "Frame number", 
            ylab = ylab, xaxt = "n")
        xAxisLabels <- c(data$frame[1], (data$frame[1] + round((data$frame[length(data$frame)]/14) * 1, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            2, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 3, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            4, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 5, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            6, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 7, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            8, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 9, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            10, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 11, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            12, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 13, digits = 0)), data$frame[length(data$frame)])
        axis(side = 1, at = xAxisLabels, labels = xAxisLabels)
        
        legend("topright", c("X Values", "Y Values"), col = color, lty = 1)
    } else {
        legend("bottomright", c("X Values", "Y Values"), col = color, lty = 1)
    }

    # If present, draw stimuli episodes as labeled rectangles 
    if(length(stimF) > 1) {
      # Search for the first and last frame of an episode
      stimuli <- subset(unique(stimF), subset = (unique(stimF) != ""))
      # Preallocate data frame
      stimSSFrames <- as.data.frame(matrix(data = NA, nrow = length(stimuli), ncol = 2))
      names(stimSSFrames) <- c("start","stop")

      # Get start & stop frames per stimulus
      for(i in 1:length(stimuli)) {
        temp <- subset(data, subset = (data$stimF == stimuli[i]))
        stimSSFrames[i,"start"] <- temp$frames[1]
        stimSSFrames[i,"stop"] <- temp$frames[nrow(temp)]
      }
      
      # Draw the rectangles per stimulus
      for(i in 1:length(stimuli)) {
        rect(xleft = stimSSFrames[i,"start"], ybottom = min, xright = stimSSFrames[i,"stop"], ytop = max, lty = 3, lwd = .5, col = "transparent")
        text(x = ((stimSSFrames[i,"stop"] - stimSSFrames[i,"start"])/2 + stimSSFrames[i,"start"]), y = ((max - min)/3 + min), labels = stimuli[i], col = "darkgrey")
      }
    }
    
    # Draw marker movement data in basic plot
    points(data$frames, data$xMarker, type = "l", col = color[1])
    points(data$frames, data$yMarker, type = "l", col = color[2])
}
