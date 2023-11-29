#' Plot markers on symbolic head
#' 
#' Plots a symbolic, standardized head with markers plotted at predefined 
#' starting points. To get meaningful plots, functions 
#' \code{\link{face2stdFace}} and \code{\link{centerCond}} first.
#' 
#' @param data Data-frame with a row sequence of standardized and centered x/y
#'   coordinates for each marker. Please use the functions 
#'   \code{\link{face2stdFace}} and \code{\link{centerCond}} 
#'   before plotting the data. Names of the data columns 
#'   must end in '_x' for the x-axis and in '_y' for the y-axis.
#' @param dataPos Named list of x/y starting position coordinates for each marker which
#'   should be plotted on the standardized face (e.g., dataPos = 
#'   c(BL2 = c(-.3,.7), BR2 = c(.3,.7)), ...). Due to the tracking
#'   procedure in blender, the center of the coordinate system is the nose tip 
#'   at x = 0, y = 0. 
#' @param title Default is no title.
#' @param color Character vector of length one to define the color for plotting
#'   the Marker. Default is 'black'.
#' @param alpha Alpha value / density for marker plots. 0 represents full 
#' transparency, whereas 1 represents full color. Default = .50. This may be 
#'   adapted for plotting many participants
#' @param overplot If TRUE, only the marker plots, but no base graph is plotted.
#'   Default is FALSE.
#' @param plotDataPos If TRUE, for debugging puposes, the start position 
#'   coordinates of dataPos are plotted. Default is FALSE.
#' @param plotDotSize Size of the dots in cex. Default is 0.2.
#'   
#' @import grDevices
#' 
#' @return See vignette for example plots.
#'   
#' @author Rainer Alexandrowicz \email{rainer.alexandrowicz@aau.at}, 
#' Axel Zinkernagel \email{zinkernagel@uni-wuppertal.de}
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
#'# Select data for plotting (selecting stimulus type and omit z-axis)
#'data_Subj_happy <- subset(dataStdFCen, subset = (dataStdFCen$Stimulustype == "posed_happy"), 
#'                          select = c("subject",colNames))
#'data_Subj_disgust <- subset(dataStdFCen, subset = (dataStdFCen$Stimulustype == "posed_disgust"), 
#'                            select = c("subject",colNames))
#'
#'# Define the positions for the markers for the standardized face of x (-1,1) 
#'# and y (-1,1) size as named list
#'dataPos <- list(BL2 = c(-.3,.7), BR2 = c(.3,.7), 
#'                DL2 = c(-.7,.7), DR2 = c(.7,.7), 
#'                BL4 = c(-.2,.2), BR4 = c(.2,.2), 
#'                CL4 = c(-.5,.2), CR4 = c(.5,.2), 
#'                BL5 = c(-.2,-.1), BR5 = c(.2,-.1), 
#'                BL7 = c(-.2,-.6), BR7 = c(.2,-.6), 
#'                CL7 = c(-.3,-.7), CR7 = c(.3,-.7), 
#'                A7 = c(0,-.55), 
#'                A8 = c(0,-.8)) 
#'
#'# For debugging purposes the marker names and start positions may also be plotted
#'plotXhead(data = data_Subj_happy[-1], dataPos = dataPos, 
#'          title = "All Subjects, happy", plotDataPos = TRUE)
#' 
#' @export
plotXhead <- function(data, dataPos, title = "", overplot = FALSE, color = "black", alpha = 0.5, plotDataPos = FALSE, plotDotSize = .02) {
    # Error handling
    if (!(nrow(data)) > 0 | !(is.data.frame(data))) {
        stop("Argument data is missing or of incorrect type!")
    }
    if (!(is.list(dataPos))) {
        stop("Argument dataPos is not a named list!")
    } else {
      if (!unique(names(dataPos) %in% unique(substr(names(data), 1, nchar(names(data)) - 2)))) {
        stop("Argument dataPos does not have the same marker names as data dataframe has!")
      }
    }
    if (!(is.character(title))) {
        stop("Argument title is not of type character!")
    }
    if (!(is.character(color))) {
        stop("Argument color is of incorrect type!")
    }
    if (!(is.numeric(alpha) | alpha < 0 | alpha > 1)) {
        stop("Argument alpha is not of type numeric between 0 and 1!")
    }
    if (!(is.logical(overplot))) {
        stop("Argument overplot is not of type logical!")
    }
    
    # Helper functions
    plotBaseXHead <- function() {
        # the hard coded plot area of -1 / 1 in x / y-axis direction shoud be enough,
        # because face2stdFace scaled data has pupil-pupil and left-pupil-left-mouthcorner distance of 1/3
        plot(0:1, 0:1, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", axes = F, main = title, xlab = "", ylab = "")  # plot area
        box(which = "plot")
        abline(h = 0, col = grey(0.8), lty = 3)  # horizontal axis
        abline(v = 0, col = grey(0.8), lty = 3)  # vertical axis
        
        # using the nose as center, as it is done in blender measurement
        points(0, 0, pch = 24, cex = 2, col = 1)  # nose (at the center)
        
        points(-.50, .50, pch = 1, cex = 1.5)  # left eye
        points(.50, .50, pch = 1, cex = 1.5)  # right eye
        points(-.50, .50, pch = 16, cex = 0.7)  # left pupil
        points(.50, .50, pch = 16, cex = 0.7)  # right pupil
        
        segments(-.30, -.70, .30, -.70, lwd = 2)  # mouth line
    }
    
    # Sort columns in data
    data <- data[sort(names(data))]
    # Sort dataPos list
    dataPos <- dataPos[sort(names(dataPos))]
    
    # add offset value for each marker to position marker to the correct position of the x-head
    tempData <- sweep(data, MARGIN = 2, as.numeric(unlist(dataPos)), FUN = "+")
    
    # plot base square head
    if (!overplot) {
        plotBaseXHead()
    }
    
    # plot marker start positions (for debugging purposes)
    if (plotDataPos) {
      for (i in 1:length(dataPos)) {
        points(dataPos[[i]][1], dataPos[[i]][2], pch = 16, col = "gray60", cex = 0.7)
        textxy(dataPos[[i]][1],dataPos[[i]][2],names((dataPos))[i],cex = .8, col = "gray60")
      }
    }    
    
    # recompute and add alpha/transparency value to color settings
    color <- c(as.numeric(col2rgb(color)), alpha * 255)
    color <- rgb(color[1], color[2], color[3], color[4], maxColorValue = 255)
    
    # Plot markers on square head
    for (i in 1:(ncol(tempData)/2)) {
        points(as.numeric(unlist(tempData[(i * 2) - 1])), as.numeric(unlist(tempData[i * 2])), pch = 16, col = color, cex = plotDotSize)
    }
}
