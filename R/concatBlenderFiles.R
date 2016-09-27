#' Concatenate data files
#' 
#' Concatenate several csv/Rdata files into one large 'master'-file. Either csv
#' or Rdata files can be concatenated, but not mixed cvs/Rdata files. Data files need not
#' to have the same number of colums, e.g., it is possible to concatenate files with varying
#' number of markers tracked. The total number of columns will be
#' determined by this function.
#' 
#' @param dataFileNames character vector of the filenames to be concatenated. 
#' Possiblefiletypes are csv and R data files (File endings: *.csv, *.rda, 
#' *.Rdata).
#' @param inputDirectory Character variable containing the path to the input
#' files (e.g., on Windows: 'C:/Data/Blenderdata/', on Linux:
#' '/home/user/Data/Blenderdata/').
#' @param subjectColumn Logical value. Default is FALSE. Do the single data 
#' files contain a column with the subject number?  If TRUE, the subject number 
#' is taken from the single files. If FALSE, the subject number has to be part 
#' of the filename. It should be the last number before the filetype (e.g., 
#' '.RData' or '.csv') separated by an underscore from the rest of the filename.
#'  For example, from the filename 'RawData_Subj_39.RData' the subject number 
#'  39 is generated. 
#' @param outputFilename Name of the output file
#' @param outputDirectory Optional. Path to where the output file should be
#' saved (e.g., on Windows: 'C:/Data/Blenderdata/output/', on Linux:
#' '/home/user/Data/Blenderdata/output/). If empty, inputDirectory is used.
#' @param verbose If TRUE, the function prints verbose output.
#'   
#' @return Returns data frame and saves Rdata file of concatenated input files.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' \dontrun{
#' inputdir <- paste(system.file(package = "blenderFace"),"/extdata/",sep="")
#' outputdir <- paste(system.file(package = "blenderFace"),"/data/",sep="")
#' filenames <- c("Subject_01.csv","Subject_02.csv")
#' 
#' concatBlenderFiles(dataFileNames = filenames, inputDirectory = inputdir, 
#' subjectColumn = FALSE, outputFilename = "Rawdata.rda", 
#' outputDirectory = outputdir, verbose = TRUE)
#' }
#' 
#' @export
concatBlenderFiles <- function(dataFileNames, inputDirectory, subjectColumn = FALSE, outputFilename, outputDirectory = "", verbose = FALSE) {
    # Error handling
    
    # Remove leading / trailing spaces from inputDirectory / outputDirectory
    inputDirectory <- gsub("^\\s+|\\s+$", "",inputDirectory)
    outputDirectory <- gsub("^\\s+|\\s+$", "",outputDirectory)
    
    if (!is.character(dataFileNames) | !length(dataFileNames) > 1) {
        stop("Argument dataFileNames is not of type character or does not contain two or more filenames!")
    }
    if (!(is.character(inputDirectory))) {
        stop("Argument inputDirectory is not of type character!") 
        # first mode number must be either 5 or 7 to be at least readable / executable (changable) by the user
        if (!(dir.exists(inputDirectory)) | !(substr(as.character(file.mode(inputDirectory)),1,1) == "5" | substr(as.character(file.mode(inputDirectory)),1,1) == "7")) { 
            stop("The input directory is not accessible! Please check the path and the directory permissions.")
        }
    }
    if (is.character(outputDirectory)) {
      # first mode number must be either 3 or 7 to be at least writable / executable (changable) by the user
        if ((outputDirectory == "") | (!(dir.exists(outputDirectory))) | !(substr(as.character(file.mode(outputDirectory)),1,1) == "3" | substr(as.character(file.mode(outputDirectory)),1,1) == "7")) {
            stop("The output directory is not accessible! Please check the path and the directory permissions.")
        }
    } else {
        stop("Argument inputDirectory is not of type character!")
    }
    if (outputDirectory == "") {
        outputDirectory <- inputDirectory
    }
    if (substr(inputDirectory, nchar(inputDirectory), nchar(inputDirectory)) != "/") {
        # Add Slash, if needed
        inputDirectory <- paste(inputDirectory, "/", sep = "")
    }
    if (substr(outputDirectory, nchar(outputDirectory), nchar(outputDirectory)) != "/") {
        # Add Slash, if needed
        outputDirectory <- paste(outputDirectory, "/", sep = "")
    }
    if (!(is.character(dataFileNames)) | !(length(dataFileNames) > 1)) {
        stop("Argument filenames is not of type character or does not contain more than one value!")
    }
    if (!(is.logical(subjectColumn))) {
        stop("Argument subjectColumn is missing or not of type logical!")
    }
    if (!(is.character(outputFilename))) {
        stop("Argument outputFilename is not of type character!")
    }
    if (!(is.logical(verbose))) {
        stop("Argument verbose is not of type logical!")
    }
    
    # Determing file type from file ending (*.csv, *.rda, *.Rdata); only the first filename is used!
    filetype <- NULL
    if (tolower(substr(dataFileNames[1], nchar(dataFileNames[1])-2, nchar(dataFileNames[1]))) == "csv") {
        filetype <- "csv"
    }
    if ((tolower(substr(dataFileNames[1], nchar(dataFileNames[1])-2, nchar(dataFileNames[1]))) == "rda") | (tolower(substr(dataFileNames[1], nchar(dataFileNames[1])-2, nchar(dataFileNames[1]))) == "rdata")) {
        filetype <- "rda"
    }
    if (!((filetype != "csv") | (filetype != "rda"))) {
        stop("Filetypes not supperted! This function concatenates *.csv and *.rda / *.RData files.")
    }
    
    # Step 1: Determing unique column names and nrows of the files to be concatenated for preallocation of dataframe
    if (verbose) {
        writeLines("Step 1: Determing unique column names and number of rows of the files to be concatenated.")
    }
    
    NFiles <- length(dataFileNames)
    dataColNames <- NULL
    dataNrows <- 0
    if (verbose) {
      writeLines(paste("Reading", NFiles,"files:"))
    }
    for (i in 1:length(dataFileNames)) {
        if (verbose) {
            writeLines(paste("Reading file ", dataFileNames[i], " (", i, "/", NFiles, ")", sep = ""))
        }
        if (filetype == "rda") {
            # loading Rdata-files
            dataName <- load(paste(inputDirectory, dataFileNames[i], sep = ""))
            tempData <- get(dataName)
            rm(list = dataName)  # delete temp data to keep memory usage low
            gc()
        }
        if (filetype == "csv") {
            # loading csv-files
            tempData <- read.csv2(paste(inputDirectory, dataFileNames[i], sep = ""), header = TRUE, sep = ";")
        }
        if (verbose) {
            writeLines(paste("  Adding ", nrow(tempData), " rows to data frame of actually ", dataNrows, " rows.", sep = ""))
        }
        dataColNames <- c(dataColNames, names(tempData))
        dataNrows <- dataNrows + nrow(tempData)
    }
    rm(list = c("tempData"))
    dataColNames <- sort(unique(dataColNames))
    
    if (verbose) {
        writeLines(paste("\nThe final data frame will have ", 
                         if (subjectColumn == FALSE)
                         {length(dataColNames) + 1}
                         else {length(dataColNames)}, 
                         " columns and ", dataNrows, " rows.", sep = ""))
    }
    
    if (verbose) {
        writeLines("\nThese are the unique column names of all files to be concatenated. Check, if they are correct.")
        print(dataColNames)
        writeLines("Abort Script? (Press 'y' to abort, or any other key to coninue)")
        if (tolower(readline(prompt = "? ")) == "y") {
            stop("Aborted due to user request.")
        }
    }
    
    # Step 2: Concatenating files
    if (verbose) {
        writeLines("Step 2: Concatenating files.")
    }
    
    # Preallocate empty data frame
    if (verbose) {
        writeLines(paste("\nPreallocating data frame of a ", 
                         if (subjectColumn == FALSE)
                         {length(dataColNames) + 1}
                         else {length(dataColNames)}, 
                         "x", dataNrows, " matrix.", sep = ""))
    }
    if (subjectColumn) {
        rawdata <- as.data.frame(matrix(data = NA, nrow = dataNrows, ncol = length(dataColNames)))
        colnames(rawdata) <- dataColNames
    } else {
        rawdata <- as.data.frame(matrix(data = NA, nrow = dataNrows, ncol = length(dataColNames) + 1))
        colnames(rawdata) <- c("subject", dataColNames)
    }
    
    # load and concatenate files
    dataNrows <- 1  # counter must start at 1 to address dataframe rows correctly
    for (i in 1:length(dataFileNames)) {
        if (verbose) {
            writeLines(paste("Concatenating file ", dataFileNames[i], " (", i, "/", length(dataFileNames), ")", sep = ""))
        } else {
            # This process may take a while, so its better to give feedback
            writeLines(paste("Concatenating file ", dataFileNames[i], " (", i, "/", length(dataFileNames), ")", sep = ""))
        }
        if (filetype == "rda") {
            # loading Rdata-files
            dataName <- load(paste(inputDirectory, dataFileNames[i], sep = ""))
            tempData <- get(dataName)
            rm(list = dataName)  # delete temp data to keep memory usage low
        }
        if (filetype == "csv") {
            # loading csv-files
            tempData <- read.csv2(paste(inputDirectory, dataFileNames[i], sep = ""), header = TRUE, sep = ";")
        }
        
        # Compute subject number from filename
        if (!subjectColumn) {
            filenameParts1 <- strsplit(dataFileNames[i], "\\.")[[1]][1]
            filenameParts2 <- strsplit(filenameParts1, "_")[[1]]
            subject <- as.numeric(filenameParts2[length(filenameParts2)])
            
            tempData <- cbind(subject, tempData)
        }
        
        # Writing data into preallocated data frame (much faster than merge())
        rawdata[(dataNrows:(dataNrows + nrow(tempData) - 1)), names(tempData)] <- tempData
        dataNrows <- dataNrows + nrow(tempData)
    }
    rm(tempData)
    
    # Step 3: Save output file
    if (verbose) {
        writeLines("Step 3: Saving output file (it takes time to save large files).")
    }
    
    # Sorting rows according to subject
    rawdata <- rawdata[with(rawdata, order(rawdata$subject)), ]
    # Sorting columns alphabetically
    rawdata <- rawdata[sort(names(rawdata))]
    
    save(rawdata, file = paste(outputDirectory, outputFilename, sep = ""))
}
