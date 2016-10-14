#' Plots median movement of a marker per subject
#' 
#' Plots median movement of a marker per subject and labels it with subject 
#' number. This function may be used to find individual outliers
#' 
#' @param data Data frame containig the data to be plotted
#' @param colNames Character vector naming the two columns with the x/y-Values
#'   to be plotted
#' @param colNameSubj Character vector with a single value containig the name 
#' of the subject column of the data data-frame.
#' @param title Character string containing the title. Default is no title.
#' @param xlim Numeric vector of two elements defining the x-axis range of the 
#' plot. Default is c(-10, 10).
#' @param ylim Numeric vector of two elements defining the y-axis range of the 
#' plot. Default is c(-10, 10).
#' @param verbose print verbose output
#'   
#' @return Plot of median movement of marker with quartile ellipse around per
#'   condition
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' \dontrun{plotMmpCond()}
#'    
#' @export
plotIndmm <- function(data, colNames, colNameSubj, title = "", xlim = c(-10, 10),
ylim = c(-10, 10), verbose = FALSE) {
    # Error handling
    if (!(is.data.frame(data))) {
        stop("Argument data does not contain a data frame!")
    }
    if (!(is.character(colNames)) | length(colNames) != 2) {
      stop("Argument colNames is missing, not of type character, or contains not two names (x/y columns)!")
    }
    if (!(is.character(colNameSubj)) | length(colNameSubj) != 1) {
      stop("Argument colNameSubj is not of type character!")
    }
    if (!(is.character(title))) {
      stop("Argument title is not of type character!")
    }
    if (!(is.numeric(xlim)) | length(xlim) != 2) {
      stop("Argument xlim is not numeric or not containing two elements")
    }
    if (!(is.numeric(ylim)) | length(ylim) != 2) {
      stop("Argument ylim is not numeric or not containing two elements")
    }
    if (!(is.logical(verbose))) {
      stop("Argument verbose is not of type logical!")
    }
    
    # Axel: fix me from here:
    # - einheitliche funktions-parammeter
    # z.B. istplotXYmmpf anders (xMarker, yMarker)
    # als plotMmpCond (data, colNames)
    
    
    ##########
    # Ab hier Funktionsvorlage:
    # x: x-Coordinates
    # y: y-Coordinates
    # labels: labels for xy-Points
    # plotLabels: c(xlab, ylab, main, sub)
    # pdfName: name and path to save the pdf
    
        
    # compute max / min-values for x / y -axes
#     if (max(x,na.rm = TRUE) <= 0) {
#       max_x <- 1 
#     } else {
#       max_x <- max(x,na.rm = TRUE) + 1  
#     }
#     if (min(x,na.rm = TRUE) >= 0) {
#       min_x <- -1
#     } else {
#       min_x <- min(x,na.rm = TRUE) - 1  
#     }
#     if (max(y,na.rm = TRUE) <= 0) {
#       max_y <- 1 
#     } else {
#       max_y <- max(y,na.rm = TRUE) + 1  
#     }
#     if (min(y,na.rm = TRUE) >= 0) {
#       min_y <- -1
#     } else {
#       min_y <- min(y,na.rm = TRUE) - 1  
#     }
#     
#     plot(c(min_x,max_x),c(min_y,max_y),type="n",xlab=plotLabels[1],ylab=plotLabels[2],main=plotLabels[3],sub=plotLabels[4])
#     points(0,0,pch=19,col=1)
#     points(x,y, pch=19, col=1)
#     for (i in 1:length(x)) {
#       lines(c(0,x[i]),c(0,y[i]),type="l",col=1)
#       if(!is.na(x[i])) {textxy(x[i],y[i],labels[i],cex=1)}
#     }
}
