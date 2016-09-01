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
#' \dontrun{centerCond()}
#' 
#' @import doParallel
#' 
#' @return List of (1) data frame with centered columns and (2) data frame with 
#' individual offset values. The latter may be used to set the markers at
#' measured locations in standardized face plots.
#'   
#' @export
centerCond <- function(data, colNames, colNameSubj, colNameFrames, colNameCond, verbose = FALSE) {
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
    
    # Reordering columns of input data frame
    data <- data[c(colNameSubj, colNameFrames, colNameCond, colNames)]
    
    ############################## Setting up CPU-Cluster Leaving one CPU core for OS tasks
    if (parallel::detectCores() > 1) {
        cl <- parallel::makeCluster(detectCores() - 1)
        doParallel::registerDoParallel(cl)
    }
    if (verbose) {
        writeLines(paste("Starting up CPU-cluster: Using ", getDoParWorkers(), " CPU-cores, leaving one for the OS.", sep = ""))
        timestamp0 <- Sys.time()
    }
    
    ############################## Helper Functions
    
    # Find first frame of conditions per subject
    FirstFrameSubjCond <- function(subjCol, frameCol, condCol) {
        # SubjCol: column containing the subject number FrameCol: column containing the frame numbers CondCol: column containing the
        # conditions, for which the first occurrence is searched for
        
        data <- data.frame(subjCol, frameCol, condCol)
        rm(list = "subjCol", "frameCol", "condCol")
        subjects <- unique(data$subjCol)
        
        # Verbose output is very time consuming; if wanted, add '.verbose = verbose' to the first foreach loop
        firstFrames <- foreach(i = subjects, .combine = rbind) %dopar% {
            tempData <- subset(data, subset = (data$subjCol == i))
            output <- tempData[!duplicated(tempData$condCol), ]
            return(output)
        }
        return(firstFrames)
    }
    
    # Find length of conditions per subject
    LengthSubjCond <- function(subjCol, condCol) {
        # SubjCol: column containing the subject number CondCol: column containing the conditions, for which the first occurrence is searched
        # for
        
        data <- data.frame(subjCol, condCol)
        rm(list = "subjCol", "condCol")
        subjects <- unique(data$subjCol)
        
        cond <- subset(unique(data$condCol), subset = (unique(data$condCol) != ""))
        
        # prevent j beeing a global variable (devtools, check)
        j <- NULL
        # Verbose output is very time consuming; if wanted, add '.verbose = verbose' to the first foreach loop
        lengthFrames <- foreach(i = subjects, .combine = cbind) %:% foreach(j = cond, .combine = cbind) %dopar% {
            c(i, j, nrow(subset(data, subset = (data$subjCol == i & data$condCol == j))))
        }
        lengthFrames <- as.data.frame(t(lengthFrames))
        names(lengthFrames) <- c("subjCol", "condCol", "condLengthCol")
        lengthFrames$subjCol <- as.numeric(lengthFrames$subjCol)
        lengthFrames$condLengthCol <- as.numeric(lengthFrames$condLengthCol)
        return(lengthFrames)
    }
    
    ############################## Step 1: Getting condition start frames per subject
    if (verbose) {
        writeLines("Step 1: Getting condition start frames per subject.")
        timestamp1 <- Sys.time()
    }
    condStartFrames <- FirstFrameSubjCond(data$subject, data$Frame, data$stimulustype)
    
    # removing 'empty' condition starts
    condStartFrames <- subset(condStartFrames, subset = (condStartFrames$condCol != ""))
    
    # getting the frame length of the conditions and attaching it to condStartFrames
    condLengthFrames <- LengthSubjCond(data$subject, data$stimulustype)
    condStartFrames <- cbind(condStartFrames, condLengthCol = condLengthFrames$condLengthCol)
    rm(condLengthFrames)
    
    ############################## Step 2: Getting offset values for the to be centered variables for each subject and each condition
    if (verbose) {
        writeLines("Step 2: Getting offset values per condition per subject.")
        timestamp2 <- Sys.time()
    }
    # Verbose output is very time consuming; if wanted, add '.verbose = verbose' to the first foreach loop
    offsetValues <- foreach(i = 1:nrow(condStartFrames), .combine = rbind) %dopar% {
        offsetRow <- subset(data, subset = ((data[[colNameSubj]] == condStartFrames$subjCol[i]) & (data[[colNameFrames]] == condStartFrames$frameCol[i])), 
            select = colNames)
    }
    
    offsetTable <- cbind(condStartFrames, offsetValues)
    rm(list = c("condStartFrames", "offsetValues"))
    
    ############################## Step 3: compute centered values for all columns and subjects
    if (verbose) {
        writeLines("Step 3: Subtracting offset values per condition per subject.")
        timestamp3 <- Sys.time()
    }
    
    # First loop over rows in offsetTable (i), Second loop over colNames (j) Verbose output is very time consuming; if wanted, add
    # '.verbose = verbose' to the first foreach loop prevent j beeing a global variable (devtools, check)
    j <- NULL
    dataCen <- foreach(i = 1:nrow(offsetTable), .combine = rbind) %:% foreach(j = 1:length(colNames), .combine = cbind) %dopar% {
        subset(data, subset = ((data[[colNameSubj]] == offsetTable[i, "subjCol"]) & (data[[colNameCond]] == offsetTable[i, "condCol"])), 
            select = colNames[j]) - offsetTable[i, colNames[j]]
    }
    
    # Compute subject and condition columns and cbind it to dataCen fix me: Preallocate vectors
    subjVec <- NULL
    framesVec <- NULL
    condVec <- NULL
    for (i in 1:nrow(offsetTable)) {
        subjVec <- c(subjVec, rep(offsetTable$subjCol[i], offsetTable$condLengthCol[i]))
        framesVec <- c(framesVec, seq.int(offsetTable$frameCol[i], (offsetTable$frameCol[i] + (offsetTable$condLengthCol[i] - 1))))
        condVec <- c(condVec, rep(as.character(offsetTable$condCol[i]), offsetTable$condLengthCol[i]))
    }
    
    dataCen <- data.frame(subjVec, framesVec, condVec, dataCen, stringsAsFactors = FALSE)
    names(dataCen) <- c(colNameSubj, colNameFrames, colNameCond, colNames)
    
    ############################## Step 4: replace original values with centered values
    
    if (verbose) {
        writeLines("Step 4: Replacing centered values in the original data.")
        timestamp4 <- Sys.time()
    }
    
    # Add data rows without a condition and order the data frame by subject,frames
    dataCen <- rbind(dataCen, subset(data, subset = ((data[[colNameCond]] == "") | (is.na(data[[colNameCond]])))))
    dataCen <- dataCen[with(dataCen, order(dataCen[[colNameSubj]], dataCen[[colNameFrames]])), ]
    
    ############################## Plausibility checks and performance output
    
    # Check of plausibility: are all condition onset rows at 0?
    if (verbose) {
        subjects <- unique(dataCen$subject)
        conditions <- unique(dataCen$stimulustype)
        conditions <- subset(conditions, subset = (conditions != ""))
        
        testmatrix <- foreach(i = 1:length(subjects), .combine = rbind) %:% foreach(j = 1:length(conditions), .combine = rbind) %dopar% 
            {
                head(subset(dataCen, subset = (dataCen$subject == subjects[i] & dataCen$stimulustype == conditions[j])), 1)
            }
        
        writeLines("\nPlausibility check: ColSums of centered start frames per condition should be 0:")
        print(colSums(testmatrix[, colNames], na.rm = TRUE))
    }
    
    if (verbose) {
        timestamp5 <- Sys.time()
        writeLines("\nTime for completing each step of the function:")
        writeLines("Step1: Getting condition start frames per subject: ")
        print(round(timestamp2 - timestamp1, 2))
        writeLines("Step2: Getting offset values per condition per subject: ")
        print(round(timestamp3 - timestamp2, 2))
        writeLines("Step3: Subtracting offset values per condition per subject: ")
        print(round(timestamp4 - timestamp3, 2))
        writeLines("Step4: Replacing centered values in the original data: ")
        print(round(timestamp5 - timestamp4, 2))
        writeLines("Overall time: ")
        print(round(timestamp5 - timestamp0, 2))
    }
    
    ############################## Finish
    
    if (verbose) {
        writeLines("Shutting down CPU-Cluster")
    }
    stopCluster(cl)
    
    return(list(dataCen, offsetTable))
}
