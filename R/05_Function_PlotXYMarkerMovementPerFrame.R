#' Plot marker movement per frame
#' 
#' Plots separate lines for the x,y marker values (y-axis) per frame (x-axis).
#' 
#' @param frames numeric vector containing the frames for which x/y-values
#'   should be plotted
#' @param xMarker numeric vector with x values (same length as frames vector)
#' @param yMarker numeric vector with y values (same length as frames vector)
#' @param center if TRUE, center x/y values by setting the x/y values to 0 at
#'   the first (appropriate, e.g. non NA frame) frame. Default = TRUE
#' @param color character vector of two values to define color for x/y values of
#'   the Marker. Default is c('red','orange').
#' @param title Default is no title.
#' @param overplot If TRUE, only the x/y lines, but no base graph is plotted.
#'   Default is FALSE.
#'   
#' @return nice Plot
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' \dontrun{PlotXYMarkerMovementPerFrame()}
#'   
#' @export
PlotXYMarkerMovementPerFrame <- function(frames, xMarker, yMarker, center = TRUE, color = c("red", "orange"), title = "", overplot = FALSE) {
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
    
    # Begin plot function
    data <- data.frame(frames, xMarker, yMarker)
    rm("frames", "xMarker", "yMarker")
    
    if (center) {
        minX <- -0.05
        maxX <- 0.05
        minY <- -0.05
        maxY <- 0.05
        ylab <- "Centered deviation"
    } else {
        minX <- min(data$xMarker, na.rm = TRUE)
        maxX <- max(data$xMarker, na.rm = TRUE)
        minY <- min(data$yMarker, na.rm = TRUE)
        maxY <- max(data$yMarker, na.rm = TRUE)
        ylab <- "Deviation"
    }
    min <- min(c(minX, minY))
    max <- max(c(maxX, maxY))
    
    if (!overplot) {
        plot(1, type = "n", xlim = c(data$frame[1], data$frame[length(data$frame)]), ylim = c(min, max), main = title, xlab = "Frame number", ylab = ylab, 
            xaxt = "n")
        xAxisLabels <- c(data$frame[1], (data$frame[1] + round((data$frame[length(data$frame)]/14) * 1, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            2, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 3, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            4, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 5, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            6, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 7, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            8, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 9, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            10, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 11, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 
            12, digits = 0)), (data$frame[1] + round((data$frame[length(data$frame)]/14) * 13, digits = 0)), data$frame[length(data$frame)])
        axis(side = 1, at = xAxisLabels, labels = xAxisLabels)
        
        legend("topright", c("X Values", "Y Values"), col = color, lty = 1)
    }
    
    if (overplot) {
        legend("bottomright", c("X Values", "Y Values"), col = color, lty = 1)
    }
    
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
    
    if (center) {
        data$xMarker <- data$xMarker - minValidFrameValue(data$frames, data$xMarker)
        data$yMarker <- data$yMarker - minValidFrameValue(data$frames, data$yMarker)
    }
    
    points(data$frames, data$xMarker, type = "l", col = color[1])
    points(data$frames, data$yMarker, type = "l", col = color[2])
} 
