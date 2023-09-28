#' Concatenate Blender data files
#' 
#' Concatenate several csv or Rdata files into one large 'master'-file. Either csv
#' or Rdata files can be concatenated, however, not mixed cvs/Rdata files. The data 
#' files must all reside in the same directory. The files need not to have the 
#' same number of colums, e.g., it is possible to concatenate files with a different
#' number of markers tracked. The total number of columns will be
#' determined during runtime of this function. Before concatenating the files, 
#' a feedback of the columns for the final data set is given and asked for it's
#' correctness.
#' 
#' @param dataFileNames Character vector containing filenames to be concatenated. 
#' Possible file types are csv and R data files (file extensions: *.csv, *.rda, 
#' *.Rdata).
#' @param inputDirectory Character vector of length one containing the path to the 
#' directory of data files (e.g., on Windows: 'C:/Data/Blenderdata/', on 
#' Unix-type systems: '/home/user/Data/Blenderdata/').
#' @param colNameSubj Character vector of length one, default is empty (""). 
#' If empty, the subject number is taken from the last number in the file names 
#' of the single data files, which should be concatenated (in front of the file 
#' extension). For example, from the filename 'RawData_Subj_39.RData' the subject number 
#'  39 is taken. If the subject number is contained the single data files, the
#' colNameSubj parameter must contain the column name of the subject column of
#' the data files (e.g., \code{colNameSubj = "subject"}).
#' @param verbose Logical value. If TRUE, the function provides verbose console output.
#'
#' @import utils 
#'         
#' @return Returns data frame and saves Rdata file of concatenated input files.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-wuppertal.de},
#' Rainer Alexandrowicz \email{rainer.alexandrowicz@aau.at}
#'   
#' @examples
#' \dontrun{
#' inputdir <- paste(system.file(package = "blenderFace"),"/extdata/",sep="")
#' outputdir <- paste(system.file(package = "blenderFace"),"/data/",sep="")
#' filenames <- c("Subject_01.csv","Subject_02.csv")
#' 
#' concatBlenderFiles(dataFileNames = filenames, inputDirectory = inputdir, 
#' colNameSubj = "", verbose = TRUE)
#' }
#' 
#' @export
concatBlenderFiles <- function(dataFileNames, inputDirectory, colNameSubj = "", verbose = FALSE) {
  # Error handling
  
  # Remove leading / trailing spaces from inputDirectory
  inputDirectory <- gsub("^\\s+|\\s+$", "",inputDirectory)
  
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
  if (substr(inputDirectory, nchar(inputDirectory), nchar(inputDirectory)) != "/") {
    # Add Slash, if needed
    inputDirectory <- paste(inputDirectory, "/", sep = "")
  }
  if (!(is.character(dataFileNames)) | !(length(dataFileNames) > 1)) {
    stop("Argument filenames is not of type character or does not contain more than one value!")
  }
  if (!is.character(colNameSubj)) {
    stop("Argument colNameSubj is not of type character!")
  }
  if (!(is.logical(verbose))) {
    stop("Argument verbose is not of type logical!")
  }
  
  # Determing file type from file ending (*.csv, *.rda, *.Rdata); only the first filename is used!
  filetype <- NULL
  if (tolower(substr(dataFileNames[1], nchar(dataFileNames[1]) - 2, nchar(dataFileNames[1]))) == "csv") {
    filetype <- "csv"
  }
  if ((tolower(substr(dataFileNames[1], nchar(dataFileNames[1]) - 2, nchar(dataFileNames[1]))) == "rda") | (tolower(substr(dataFileNames[1], nchar(dataFileNames[1]) - 4, nchar(dataFileNames[1]))) == "rdata")) {
    filetype <- "rda"
  }
  if (!((filetype != "csv") | (filetype != "rda"))) {
    stop("Filetypes not supperted! This function concatenates *.csv and *.rda / *.RData files.")
  }
  
  # Helper functions
  fcat <- function(...,newline=TRUE) {if (newline) cat(...,"\n") else cat(...); flush.console() }  # immediate console output
  
  # Step 1: Determing unique column names and nrows of the files to be concatenated for preallocation of dataframe
  # Axel: fix me: compute also unique Stimulustypes
  if (verbose) {
    fcat("Step 1: Determine unique column names and number of rows of the files to be concatenated.")
  }
  
  NFiles <- length(dataFileNames)
  dataColNames <- NULL
  dataNrows <- 0
  if (verbose) {
    fcat(paste("Read", NFiles,"files:"))
  }
  for (i in 1:length(dataFileNames)) {
    if (verbose) {
      fcat(paste("Read file ", dataFileNames[i], " (", i, "/", NFiles, ")", sep = ""))
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
      tempData <- read.table(paste0(inputDirectory, dataFileNames[i]), header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ".")
    }
    if (verbose) {
      fcat(paste("  Add ", nrow(tempData), " rows to data frame of actually ", dataNrows, " rows.", sep = ""))
    }
    dataColNames <- c(dataColNames, names(tempData))
    dataNrows <- dataNrows + nrow(tempData)
  }
  rm(list = c("tempData"))
  dataColNames <- sort(unique(dataColNames))
  
  if (verbose) {
    fcat(paste("\nThe final data frame will have ", 
               if (colNameSubj == "")
               {length(dataColNames) + 1}
               else {length(dataColNames)}, 
               " columns and ", dataNrows, " rows.", sep = ""))
  }
  
  if (verbose) {
    fcat("\nThese are the unique column names of all files to be concatenated. Check whether they are correct.")
    print(dataColNames)
    fcat("Abort Script? (Press 'y' to abort or any other key to continue)")
    if (tolower(readline(prompt = "? ")) == "y") {
      stop("Aborted due to user request.")
    }
  }
  
  # Step 2: Concatenating files
  if (verbose) {
    fcat("Step 2: Concatenate files.")
  }
  
  # Preallocate empty data frame
  if (verbose) {
    fcat(paste("\nPreallocat data frame of a ", 
               if (colNameSubj == "")
               {length(dataColNames) + 1}
               else {length(dataColNames)}, 
               "x", dataNrows, " matrix.", sep = ""))
  }
  if (colNameSubj == "") {
    rawdata <- as.data.frame(matrix(data = NA, nrow = dataNrows, ncol = length(dataColNames) + 1))
    colnames(rawdata) <- c("subject", dataColNames)
  } else {
    rawdata <- as.data.frame(matrix(data = NA, nrow = dataNrows, ncol = length(dataColNames)))
    colnames(rawdata) <- dataColNames
  }
  
  # load and concatenate files
  dataNrows <- 1  # counter must start at 1 to address dataframe rows correctly
  for (i in 1:length(dataFileNames)) {
    if (verbose) {
      fcat(paste("Concatenate file ", dataFileNames[i], " (", i, "/", length(dataFileNames), ")", sep = ""))
    } else {
      # This process may take a while, so its better to give feedback
      fcat(paste("Concatenate file ", dataFileNames[i], " (", i, "/", length(dataFileNames), ")", sep = ""))
    }
    if (filetype == "rda") {
      # loading Rdata-files
      dataName <- load(paste(inputDirectory, dataFileNames[i], sep = ""))
      tempData <- get(dataName)
      rm(list = dataName)  # delete temp data to keep memory usage low
    }
    if (filetype == "csv") {
      # loading csv-files
      tempData <- read.table(paste(inputDirectory, dataFileNames[i], sep = ""), header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ".")
    }
    
    # Compute subject number from filename
    if (colNameSubj == "") {
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
  
  # Step 3: Return output dataframe
  if (verbose) {
    fcat("Step 3: Returning output dataframe (may take some time if input data is large).")
  }
  
  # Sorting rows according to subject
  rawdata <- rawdata[with(rawdata, order(rawdata$subject)), ]
  # Sorting columns alphabetically
  rawdata <- rawdata[sort(names(rawdata))]
  
  return(rawdata)
}
