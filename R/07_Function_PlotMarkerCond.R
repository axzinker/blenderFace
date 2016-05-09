#' Plot marker and quartile ellipse per condition
#' 
#' Plots the median and quartile ellipse per experimental condition
#' 
#' @param data data frame containig the data to be plotted
#' @param colNames character vector naming the two columns with the x/y-Values
#'   to be plotted
#' @param colNameCond character vector naming the column containig the
#'   conditions
#' @param title Default is no title.
#' @param verbose print verbose output
#'   
#' @return Plot of median movement of marker with quartile ellipse around per
#'   condition
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#'   #@include plotrix
#'   
#' @examples
#' \dontrun{PlotXYMarkerMovementPerFrame()}
#'    
#' @export
PlotMarkerCond <- function(data, colNames, colNameCond, title = "", verbose = FALSE) {
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
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    # load libraries library(plotrix) # needed for draw.ellipse
    
    # Extracting conditions
    cond <- unique(na.omit(unlist(data[colNameCond])))
    cond <- cond[nchar(cond) > 0]
    
    # Generate basic plot
    plot(c(-7, 7), c(-7, 7), type = "n", xlab = "Median movement x-Axis", ylab = "Median movement y-Axis", main = title)
    legend("topright", cex = 0.7, inset = 0, cond, col = 1:length(cond), pch = 19, horiz = FALSE)
    points(0, 0, pch = 19, col = 1)
    # Draw points, lines and ellipses per condition
    for (j in 1:length(cond)) {
        if (verbose) {
            writeLines(paste("Plotting condition '", cond[j], "'", sep = ""), sep = "\n")
        }
        medianX <- median(data[data[colNameCond] == cond[j], colNames[1]], na.rm = TRUE)
        medianY <- median(data[data[colNameCond] == cond[j], colNames[2]], na.rm = TRUE)
        quantX <- quantile(data[data[colNameCond] == cond[j], colNames[1]], c(0.25, 0.75), na.rm = TRUE)
        quantY <- quantile(data[data[colNameCond] == cond[j], colNames[2]], c(0.25, 0.75), na.rm = TRUE)
        lines(c(0, medianX), c(0, medianY), type = "l", col = j)
        points(medianX, medianY, pch = 19, col = j)
        plotrix::draw.ellipse(medianX, medianY, a = abs(quantX[2]), b = abs(quantY[2]), segment = c(0, 90), arc.only = FALSE, border = j, lty = 1, lwd = 1, 
            deg = TRUE)
        plotrix::draw.ellipse(medianX, medianY, a = abs(quantX[1]), b = abs(quantY[2]), segment = c(90, 180), arc.only = FALSE, border = j, lty = 1, lwd = 1, 
            deg = TRUE)
        plotrix::draw.ellipse(medianX, medianY, a = abs(quantX[1]), b = abs(quantY[1]), segment = c(180, 270), arc.only = FALSE, border = j, lty = 1, 
            lwd = 1, deg = TRUE)
        plotrix::draw.ellipse(medianX, medianY, a = abs(quantX[2]), b = abs(quantY[1]), segment = c(270, 360), arc.only = FALSE, border = j, lty = 1, 
            lwd = 1, deg = TRUE)
    }
} 
