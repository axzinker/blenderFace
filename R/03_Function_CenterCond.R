#' Center marker coordinates
#' 
#' Sets the x/y/z-values of markers in colNames defined variable columns to 0 at
#' the start of every condition defined in variable cond. This is done for every
#' subject marked in column colNameSubj.
#' 
#' @param data Data frame containig the columns which should be centered.
#'   Remaining colums will be returned untouched.
#' @param colNames Character vector with names of the columns which should be
#'   centered.
#' @param colNameSubj Character vector with a single value containig the name of
#'   the subject column. It is assumed, that each experimental condition
#'   occurres only once per subject
#' @param colNameFrames Character vector with a single value containig the name
#'   of the frames column.
#' @param colNameCond Character vector with a single value containing the
#'   experimental conditions column name of the data frame.
#' @param verbose If TRUE, the function prints verbose output. Otherwise not.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' \dontrun{CenterCond()}
#' 
#' @import doParallel
#' 
#' @return Data frame with centered columns.
#'   
#' @export
CenterCond <- function(data, colNames, colNameSubj, colNameFrames, colNameCond, verbose = FALSE) {
    # Error handling
    if (!(is.data.frame(data))) {
        stop("Argument data does not contain a data frame!")
    }
    if (!(is.character(colNames))) {
        stop("Argument colNames is missing or not of type character!")
    }
    if (!(is.character(colNameSubj))) {
        stop("Argument colNameSubj is not of type character!")
    }
    if (!(is.character(colNameFrames))) {
        stop("Argument colNameFrames is not of type character!")
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    # Sorting data frame (subject, frame)
    data <- data[order(data[colNameSubj], data[colNameFrames]), ]
    
    # Extracting conditions
    cond <- unique(na.omit(unlist(data[colNameCond])))
    cond <- cond[nchar(cond) > 0]
    
    # Find first valid frame (which is not NA) per subject for a given condition for a given column
    minSubjCondFrame <- function(subj, col, cond) {
        # subj: actual subject number col: column for which minimum of a condition should be returned cond: condition for which the minimum is searched for
        
        # Step 1: select subject only
        tempData <- subset(data, subset = (data[colNameSubj] == subj))
        # Step 2: select condition only
        if (length(cond) > 1) {
            tempData <- subset(data, subset = (data[colNameCond] == cond))
        }
        ## Step 3: be sure, the data frame is ordered by frames (already done in line 74) tempData <- tempData[order(tempData[colNameFrames]),] Step 4: omit
        ## NAs
        tempData <- na.omit(tempData[col])
        # Step 5: select value of col with the lowest Framenumber (that is the first line, if data frame is ordered by frames)
        tempData <- tempData[1, ]
        return(tempData)
    }
    
    # Axel ToDo: function probably not correct Axel ToDo: paralellize these loops, its far to slow
    subjects <- (unique(unlist(data[colNameSubj])))
    # Loop over Subjects
    for (i in 1:length(subjects)) {
        # Loop over columns to be centered
        for (j in 1:length(colNames)) {
            # Loop over experimental conditions to be centered
            for (k in 1:length(cond)) {
                if (verbose) {
                  writeLines(paste(sep = "", "Centering Vpn ", subjects[i], " Column ", colNames[j], " for Condition '", cond[k], "'"))
                }
                data[((data[[colNameSubj]] == subjects[i]) & (data[[colNameCond]] == cond[k])), colNames[j]] <- data[((data[[colNameSubj]] == subjects[i]) & 
                  (data[[colNameCond]] == cond[k])), colNames[j]] - minSubjCondFrame(subjects[i], colNames[j], cond[k])
            }
        }
    }
    return(data)
} 
