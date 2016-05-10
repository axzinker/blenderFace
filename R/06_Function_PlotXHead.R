#' Plot markers on symbolic head
#' 
#' Plots symbolic standardized head with markers plotted on predefined areas.
#' 
#' @param data matrix or data-frame with a row sequence of *standardized* x/y
#'   coordinates for each marker. Please use function 'ScaleFaceData' to
#'   standardize data.
#' @param dataPos numeric vector with x/y starting coordinates each marker
#'   should be plotted on the face. Should have equal lenght to the number of
#'   columns of data. Default is FIX ME
#' @param dataScale numeric vector of four values containing measured left pupil
#'   - right pupil distance, left mouthcorner - right mouthcorner distance, left
#'   pupil - left mouthcorner distance, and right pupil - right mouthconrer
#'   distance.
#' @param center if TRUE, center x/y values by setting the x/y values to 0 at
#'   the first (appropriate, e.g. non NA frame) frame. Default = TRUE
#' @param color character vector of lenght one to define the color for plotting
#'   the Marker. Default is 'black'.
#' @param title Default is no title.
#' @param overplot If TRUE, only the marker plots, but no base graph is plotted.
#'   Default is FALSE.
#' @param alpha alpha value / density for marker plots. Default = .99
#' @param rwMeasure Optional. Real world measure of the object diameter used to
#'   rescale. Default is the diameter of a glue dot of 8 millimeter.
#'   
#' @return nice 'Quadratschaedel' (square head) Plot
#'   
#' @author Rainer Alexandrowicz, Axel Zinkernagel
#'   \email{rainer.alexandrowicz@aau.at, zinkernagel@uni-landau.de}
#'   
#' @examples
#' \dontrun{PlotMarkerXHead()}
#' 
#' @export
PlotMarkerXHead <- function(data, dataPos, dataScale, center = TRUE, title = "", overplot = FALSE, color = "black", alpha = 0.99, rwMeasure = 8) {
    # Error handling
    if (!(nrow(data)) > 0 | (!((is.matrix(data)) | (is.data.frame(data))))) {
        stop("Argument data is missing or of incorrect type!")
    }
    if (!(is.numeric(dataPos)) | (length(dataPos) != ncol(data))) {
        stop("Argument dataPos is not numeric or not of equal length as data matrix/dataframe!")
    }
    if (!(is.numeric(dataScale)) | (length(dataScale) != 4)) {
        stop("Argument dataScale is not numeric or does not contain four values!")
    }
    if (!(is.logical(center))) {
        stop("Argument center is not of type logical!")
    }
    if (!(is.character(title))) {
        stop("Argument title is not of type character!")
    }
    if (!(is.logical(overplot))) {
        stop("Argument overplot is not of type logical!")
    }
    if (!(is.character(color))) {
        stop("Argument color is of incorrect type!")
    }
    if (!(is.numeric(alpha))) {
        stop("Argument alpha is not of numeric logical!")
    }
    
    shape <- 1
    
    # Helper functions
    oct = function(x0, y0, x1, y1, ...) {
        h = y1 - y0
        w = x1 - x0
        xa = -w/4
        xb = w/4
        ya = h/4
        yb = -h/4
        x = c(x0, x0, xa, xb, x1, x1, xb, xa)
        y = c(ya, yb, y0, y0, yb, ya, y1, y1)
        polygon(x, y, ...)
    }  # --- end oct
    
    
    drawhair = function(xx0, yy0, xx1, yy1, dens = 100) {
        x = runif(dens, xx0, xx1)
        segments(x, yy0, x + x/10, yy1)
    }  # --- end hair
    
    x0 = -mean(dataScale[1], dataScale[2])/2  # corner points of head (via headphones)
    x1 = -x0
    y0 = -mean(dataScale[3], dataScale[4]) * 1.5
    y1 = -y0
    
    h0 = dataScale[1]/rwMeasure  # horizontal reference distance
    v0 = ((dataScale[3]/rwMeasure) + (dataScale[4]/rwMeasure))/2  # vertical reference distance
    
    hh = 3 * v0  # total height of head
    ww = 3 * h0  # total width of head
    
    mx = max(hh, ww) * 1.05/2  # plot limits
    
    # main plot function print xHead
    if (!overplot) {
        plot(0:1, 0:1, xlim = c(-mx, mx), ylim = c(-mx, mx), type = "n", axes = F, main = title, xlab = "", ylab = "")  # plot area
        box()
        abline(h = 0, col = grey(0.8), lty = 3)  # horizontal axis
        abline(v = 0, col = grey(0.8), lty = 3)  # vertical axis
        
        points(-h0/2, 0, pch = 1, cex = 1.5)  # left eye
        points(h0/2, 0, pch = 1, cex = 1.5)  # right eye
        points(-h0/2, 0, pch = 16, cex = 0.7)  # left pup
        points(h0/2, 0, pch = 16, cex = 0.7)  # right pup
        
        segments(-h0/2, -dataScale[3], h0/2, -dataScale[4], lwd = 2)  # mouth line
        
        points(0, -v0/2, pch = 24, cex = 2, col = 1)  # nose (between eyes and mouth)
        
        points(x0, 0, pch = "|", cex = 2)  # left ear
        points(x1, 0, pch = "|", cex = 2)  # right ear
        
        if (shape == 1) 
            rect(x0, y0, x1, y1, border = grey(0.7))  # head (rectangle)
        if (shape == 2) 
            oct(x0, y0, x1, y1, border = grey(0.7))  # head (octogon)
        
        drawhair(x0, y1 * 4/6, x1, y1)
    }
    
    # fix me: Plot markers on standardized Face
}
