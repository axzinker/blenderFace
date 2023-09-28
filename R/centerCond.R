#' Center marker coordinates
#' 
#' Sets the x/y/z-values of markers in parameter \code{colNames} defined 
#' variable columns to 0 at the start of every condition defined in parameter 
#' \code{colNameCond}. This is done for every subject defined by column 
#' \code{colNameSubj}.
#' 
#' @param data Data frame containig the columns which should be centered and 
#' additional columns defining subject number, stimulus conditions, frames, 
#' etc. Columns not to be centered will return unchanged.
#' @param colNames Character vector with names of the columns which should be
#'   centered.
#' @param colNameSubj Character vector with a single value containig the name of
#'   the subject column. It is assumed, that each experimental condition
#'   occurres only once per subject
#' @param colNameFrames Character vector with a single value containig the name
#'   of the frames column.
#' @param colNameCond Character vector with a single value containing the
#'   experimental conditions column name of the data frame.
#' @param maxCPUcores Optional integer value. Per default, all available CPU 
#'   cores up to 4 are used. If more or less cores should be used, set it via 
#'   the parameter \code{maxCPUcores} (e.g., \code{maxCPUcores} = 12).
#' @param verbose If TRUE, the function prints verbose output. Otherwise not.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-wuppertal.de}
#'   
#' @examples
#'colNames <- c("A7_x",  "A7_y",  "A8_x",  "A8_y",  
#'              "BL2_x", "BL2_y", "BL4_x", "BL4_y",  
#'              "BL5_x", "BL5_y", "BL7_x", "BL7_y",        
#'              "BR2_x", "BR2_y", "BR4_x", "BR4_y",  
#'              "BR5_x", "BR5_y", "BR7_x", "BR7_y",  
#'              "CL4_x", "CL4_y", "CL7_x", "CL7_y",        
#'              "CR4_x", "CR4_y", "CR7_x", "CR7_y",  
#'              "DL2_x", "DL2_y", "DR2_x", "DR2_y")
#'               
#' # To not overwrite existing data, use a new data frame 
#' # (dataStdFCen means data of standaradized faces, centered)
#' 
#' dataStdFCen <- centerCond(dataStdF, colNames = colNames, 
#'                           colNameSubj = "subject", colNameFrames = "Frame", 
#'                           colNameCond = "Stimulustype", maxCPUcores = 1,
#'                           verbose = TRUE)
#' 
#' @import doParallel
#' @import parallel
#' @import foreach
#' 
#' @return Data frame with centered columns per subject and per stimulus condition.
#'   
#' @export
centerCond <- function(data, colNames, colNameSubj, colNameFrames, colNameCond, maxCPUcores = 4, verbose = FALSE) {
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
    if (!(is.integer(maxCPUcores) | length(maxCPUcores > 1) | (maxCPUcores < 0))) {
      stop("Argument maxCPUcores is not of type positive integer!")
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    # Reordering columns of input data frame
    data <- data[c(colNameSubj, colNameFrames, colNameCond, colNames)]
    
    ############################## Setting up CPU cluster
    
    CPUavail <- parallel::detectCores()
    CPUcluster <- NA
    if (maxCPUcores <= CPUavail) {
      CPUcluster <- maxCPUcores
    } else {
      CPUcluster <- CPUavail
    }
    
    cl <- parallel::makeCluster(CPUcluster)
    doParallel::registerDoParallel(cl)
    
    if (verbose) {
        writeLines(paste("Starting up CPU cluster: Using ", getDoParWorkers(), " CPU-cores.", sep = ""))
        timestamp0 <- Sys.time()
    }
    
    ############################## Helper Functions
    
    # Find first frame of conditions per subject
    FirstFrameSubjCond <- function(subjCol, frameCol, condCol) {
        # SubjCol: column containing the subject number FrameCol: column containing the frame numbers CondCol: column containing the conditions, for
        # which the first occurrence is searched for
        
        data <- data.frame(subjCol, frameCol, condCol)
        names(data) <- c("subjCol", "frameCol", "condCol")
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
        # SubjCol: column containing the subject number CondCol: column containing the conditions, for which the first occurrence is searched for
        
        data <- data.frame(subjCol, condCol)
        rm(list = "subjCol", "condCol")
        subjects <- unique(data[,1])
        
        cond <- subset(unique(data[,2]), subset = (unique(data[,2]) != ""))
        
        # prevent j beeing a global variable (devtools, check)
        j <- NULL
        # Verbose output is very time consuming; if wanted, add '.verbose = verbose' to the first foreach loop
        lengthFrames <- foreach(i = subjects, .combine = cbind) %:% foreach(j = cond, .combine = cbind) %dopar% {
            c(i, j, nrow(subset(data, subset = (data[,1] == i & data[,2] == j))))
        }
        lengthFrames <- as.data.frame(t(lengthFrames))
        names(lengthFrames) <- c("subjCol", "condCol", "condLengthCol")
        lengthFrames$subjCol <- as.numeric(as.character(lengthFrames$subjCol))
        lengthFrames$condLengthCol <- as.numeric(as.character(lengthFrames$condLengthCol))
        return(lengthFrames)
    }
    
    ############################## Step 1: Getting condition start frames per subject
    if (verbose) {
        writeLines("Step 1: Get condition start frames per subject.")
        timestamp1 <- Sys.time()
    }
    condStartFrames <- FirstFrameSubjCond(data[colNameSubj], data[colNameFrames], data[colNameCond])
    
    # removing 'empty' condition starts
    condStartFrames <- subset(condStartFrames, subset = (condStartFrames$condCol != ""))
    
    # getting the frame length of the conditions and attaching it to condStartFrames
    condLengthFrames <- LengthSubjCond(data[colNameSubj], data[colNameCond])
    condStartFrames <- cbind(condStartFrames, condLengthCol = condLengthFrames$condLengthCol)
    rm(condLengthFrames)
    
    ############################## Step 2: Getting offset values for the to be centered variables for each subject and each condition
    if (verbose) {
        writeLines("Step 2: Get offset values per condition per subject.")
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
        writeLines("Step 3: Subtract offset values per condition per subject.")
        timestamp3 <- Sys.time()
    }
    
    # First loop over rows in offsetTable (i), Second loop over colNames (j) Verbose output is very time consuming; if wanted, add '.verbose =
    # verbose' to the first foreach loop prevent j beeing a global variable (devtools, check)
    j <- NULL
    dataCen <- foreach(i = 1:nrow(offsetTable), .combine = rbind) %:% foreach(j = 1:length(colNames), .combine = cbind) %dopar% {
        subset(data, subset = ((data[[colNameSubj]] == offsetTable[i, "subjCol"]) & (data[[colNameCond]] == offsetTable[i, "condCol"])), select = colNames[j]) - 
            offsetTable[i, colNames[j]]
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
        writeLines("Step 4: Replace centered values in the original data.")
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
        
        testmatrix <- foreach(i = 1:length(subjects), .combine = rbind) %:% foreach(j = 1:length(conditions), .combine = rbind) %dopar% {
            head(subset(dataCen, subset = (dataCen$subject == subjects[i] & dataCen$stimulustype == conditions[j])), 1)
        }
        
        writeLines("\nPlausibility check: ColSums of centered start frames per condition should be 0:")
        print(colSums(testmatrix[, colNames], na.rm = TRUE))
    }
    
    if (verbose) {
        timestamp5 <- Sys.time()
        writeLines("\nTime to complete each step of the function:")
        writeLines("Step1: Get condition start frames per subject: ")
        print(round(timestamp2 - timestamp1, 2))
        writeLines("Step2: Get offset values per condition per subject: ")
        print(round(timestamp3 - timestamp2, 2))
        writeLines("Step3: Subtract offset values per condition per subject: ")
        print(round(timestamp4 - timestamp3, 2))
        writeLines("Step4: Replace centered values in the original data: ")
        print(round(timestamp5 - timestamp4, 2))
        writeLines("Overall time: ")
        print(round(timestamp5 - timestamp0, 2))
    }
    
    ############################## Finish
    
    if (verbose) {
        writeLines("Shut down CPU cluster")
    }
    stopCluster(cl)
    
    # Eventually - later - return also the offset Table, e.g., for plotting start positions, etc.
    # return(list(dataCen, offsetTable))
    return(dataCen)
}
