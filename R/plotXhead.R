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
#'   c(AU_01_L = c(-.3,.7), AU_01_R = c(.3,.7)), ...). Due to the tracking
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
#'   
#' @return See vignette for example plots.
#'   
#' @author Rainer Alexandrowicz \email{rainer.alexandrowicz@aau.at}, 
#' Axel Zinkernagel \email{zinkernagel@uni-landau.de}
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
#' # Select data for plotting (selecting stimulus type and omit z-axis)
#' data_Subj_happy <- subset(dataStdFCen, subset = (dataStdFCen$Stimulustype == "posed_happy"), select = colNames)
#' 
#' # Define the positions for the markers for the standardized face of x (-1,1) and y (-1,1) size as named list
#' dataPos <- list(AU_01_L = c(-.3,.7), AU_01_R = c(.3,.7), AU_02_L = c(-.7,.7), AU_02_R = c(.7,.7), 
#'                 AU_06_L = c(-.5,.2), AU_06_R = c(.5,.2), AU_08 = c(0,-.55), 
#'                 AU_09_L = c(-.2,.2), AU_09_R = c(.2,.2), AU_10_L = c(-.2,-.6), AU_10_R = c(.2,-.6), 
#'                 AU_11_L = c(-.2,-.1), AU_11_R = c(.2,-.1), AU_12_L = c(-.3,-.7), AU_12_R = c(.3,-.7), 
#'                 AU_16 = c(0,-.8)) 
#' 
#' # For debugging purposes the marker names and start positions may also be plotted
#' plotXhead(data = data_Subj_happy[-1], dataPos = dataPos, title = "All Subjects, happy", plotDataPos = TRUE)
#' 
#' @export
plotXhead <- function(data, dataPos, title = "", overplot = FALSE, color = "black", alpha = 0.5, plotDataPos = FALSE) {
    # Error handling
    if (!(nrow(data)) > 0 | !(is.data.frame(data))) {
        stop("Argument data is missing or of incorrect type!")
    }
    if (!(is.list(dataPos))) {
        stop("Argument dataPos is a named list!")
    } else {
      if (as.numeric(table(unique(substr(names(data),1,nchar(names(data))-2)) == names(dataPos))["TRUE"]) != length(dataPos)) {
        stop("Argument dataPos does not have the same marker names as data dataframe has, or is not ordered in the same way!")
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
    
    # add offset value for each marker to position marker to the correct position of the x-head
    tempData <- sweep(data, MARGIN = 2, as.numeric(unlist(dataPos)), FUN = "+")
    
    # plot base square head
    if (!overplot) {
        plotBaseXHead()
    }
    
    # plot marker start positions (for debugging purposes)
    if(plotDataPos) {
      for (i in 1:length(dataPos)) {
        points(dataPos[[i]][1], dataPos[[i]][2], pch = 16, col = "gray60", cex = 0.7)
        textxy(dataPos[[i]][1],dataPos[[i]][2],names((dataPos))[i],cex=.8, col = "gray60")
      }
    }    
    
    # recompute and add alpha/transparency value to color settings
    color <- c(as.numeric(col2rgb(color)), alpha * 255)
    color <- rgb(color[1], color[2], color[3], color[4], maxColorValue = 255)
    
    # Plot markers on square head
    for (i in 1:(ncol(tempData)/2)) {
        points(as.numeric(unlist(tempData[(i * 2) - 1])), as.numeric(unlist(tempData[i * 2])), pch = 16, col = color, cex = 0.2)
    }
}
