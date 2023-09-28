#' Plot x-, y-axis marker movement per frame
#' 
#' Plots separate lines for the x,y marker values (y-axis) per frame (x-axis).
#' 
#' @param colFrames Numeric vector containing the frames for which x/y-values
#'   should be plotted
#' @param colX Numeric vector with x values (same length as frames vector)
#' @param colY Numeric vector with y values (same length as frames vector)
#' @param colCond Character vector containing the stimuli presented at each frame (same length as frames vector). Stimulus labels must be unique. Optional. Default is NA.
#' @param center If TRUE, center x/y values by setting the x/y values to 0 at
#'   the first (appropriate, e.g. non NA frame) frame. Default = TRUE
#' @param color Character vector of two values to define color for x/y values of
#'   the Marker. Default is c('red','orange').
#' @param title Default is no title.
#' @param overplot If TRUE, only the x/y lines, but no base graph is plotted.
#'   Default is FALSE.
#'   
#' @return See vignette for example plots.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-wuppertal.de}, Rainer Alexandrowicz \email{rainer.alexandrowicz@aau.at}
#'   
#' @examples
#' # Select data of a subject at relevant frame sections
#' data_Subj2Cen <- subset(dataStdFCen, subset = ((dataSmm$subject == 2) & 
#'                        (dataSmm$Frame >= 690)& (dataSmm$Frame <= 1610)))
#' plotXYmmpf(colFrames = data_Subj2Cen$Frame, colX = data_Subj2Cen$BR4_x, 
#'            colY = data_Subj2Cen$BR4_y, colCond = data_Subj2Cen$Stimulustype, 
#'            center = FALSE, title = "Subject 2, BR4")
#'   
#' @export
plotXYmmpf <- function(colFrames, colX, colY, colCond = "", center = TRUE, color = c("red", "orange"), title = "", overplot = FALSE) {
    # Error handling
    if (!(length(colFrames) > 0) | !(is.numeric(colFrames))) {
        stop("Argument colFrames missing or of incorrect type!")
    }
    if (!(is.numeric(colX)) | (length(colX) != length(colFrames))) {
        stop("Argument colX is not numeric or not of equal length as colFrames vector!")
    }
    if (!(is.numeric(colY)) | (length(colY) != length(colFrames))) {
        stop("Argument colY is not numeric or not of equal length as colFrames vector!")
    }
    if (length(colCond) > 1) {
      if (!(is.character(colCond)) | (length(colCond) != length(colFrames))) {
        stop("Argument colCond is not of type character or not of equal length as colFrames vector!")
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
    minValidFrameValue <- function(colFrames, x) {
      # colFrames: vector with frames x: vector with variable
      
      # Step 1: build data frame
      tempData <- data.frame(colFrames, x)
      # Step 2: remove NAs
      tempData <- tempData[!is.na(tempData$x), ]
      # Step 3: select minimal framenumber (= order ascending by frames)
      tempData <- tempData[order(tempData$colFrames), ]
      # Step 4: select value of col with the lowest Framenumber (that is the first line, if data frame is ordered by frames)
      tempData <- tempData$x[1]
      return(tempData)
    }
    
    # Build data frame from separate parameter data to facilitate data management
    if (length(colCond) > 1) {
      data <- data.frame(colFrames, colX, colY, colCond)
    } else {
      data <- data.frame(colFrames, colX, colY)
    }
    rm("colFrames", "colX", "colY")
    
    if (center) {
      data$colX <- data$colX - minValidFrameValue(data$colFrames, data$colX)
      data$colY <- data$colY - minValidFrameValue(data$colFrames, data$colY)
      ylab <- "Centered deviation"
    } else {
      ylab <- "Deviation"
    }
    
    # Prepare data for basic plot
    if (!is.infinite(min(data$colX, na.rm = TRUE))) {
      minX <- min(data$colX, na.rm = TRUE)
    } else {
      minX <- -0.005
    }
    if (!is.infinite(max(data$colX, na.rm = TRUE))) {
      maxX <- max(data$colX, na.rm = TRUE)
    } else {
      maxX <- 0.005
    }
    if (!is.infinite(min(data$colY, na.rm = TRUE))) {
      minY <- min(data$colY, na.rm = TRUE)
    } else {
      minY <- -0.005
    }
    if (!is.infinite(max(data$colY, na.rm = TRUE))) {
      maxY <- max(data$colY, na.rm = TRUE)
    } else {
      maxY <- 0.005
    }

    min <- min(c(minX, minY))
    max <- max(c(maxX, maxY))
    
    # Do basic plot
    if (!overplot) {
        plot(1, type = "n", xlim = c(data$colFrames[1], data$colFrames[length(data$colFrames)]), ylim = c(min, max), main = title, xlab = "Frame number", 
            ylab = ylab, xaxt = "n")
        xAxisLabels <- c(data$colFrames[1], (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 1, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 
            2, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 3, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 
            4, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 5, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 
            6, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 7, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 
            8, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 9, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 
            10, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 11, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 
            12, digits = 0)), (data$colFrames[1] + round((data$colFrames[length(data$colFrames)]/14) * 13, digits = 0)), data$colFrames[length(data$colFrames)])
        axis(side = 1, at = xAxisLabels, labels = xAxisLabels)
        
        legend("topright", c("X Values", "Y Values"), col = color, lty = 1)
    } else {
        legend("bottomright", c("X Values", "Y Values"), col = color, lty = 1)
    }
    
    # Plot a horizontal line at y = 0 (to visually check for a correct centering)
    abline(h = 0, col = "gray60")
    
    # If present, draw stimuli episodes as labeled rectangles 
    if(length(colCond) > 1) {
      # Search for the first and last frame of an episode
      stimuli <- subset(unique(colCond), subset = (unique(colCond) != ""))
      # Preallocate data frame
      stimSSFrames <- as.data.frame(matrix(data = NA, nrow = length(stimuli), ncol = 2))
      names(stimSSFrames) <- c("start","stop")

      # Get start & stop frames per stimulus
      for(i in 1:length(stimuli)) {
        temp <- subset(data, subset = (data$colCond == stimuli[i]))
        stimSSFrames[i,"start"] <- temp$colFrames[1]
        stimSSFrames[i,"stop"] <- temp$colFrames[nrow(temp)]
      }
      
      # Draw the rectangles per stimulus
      for(i in 1:length(stimuli)) {
        rect(xleft = stimSSFrames[i,"start"], ybottom = min, xright = stimSSFrames[i,"stop"], ytop = max, lty = 3, lwd = .5, col = "transparent")
        text(x = ((stimSSFrames[i,"stop"] - stimSSFrames[i,"start"])/2 + stimSSFrames[i,"start"]), y = ((max - min)/3 + min), labels = stimuli[i], col = "gray60")
      }
    }
    
    # Draw marker movement data in basic plot
    points(data$colFrames, data$colX, type = "l", col = color[1])
    points(data$colFrames, data$colY, type = "l", col = color[2])
}
