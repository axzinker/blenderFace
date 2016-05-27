#' Concatenate data files
#' 
#' Concatenate several csv/Rdata files into one large 'master'-file. Either csv
#' or Rdata files can be concatenated, but not mixed files. Data files must not
#' have the same number of colums. The exhaustive number of columns will be
#' determined by this function.
#' 
#' @param dataFileNames character vector of the filenames to be concatenated. Possible
#'   filetypes are csv and R data files (File endings: *.csv, *.rda, *.Rdata).
#' @param inputDirectory Character variable containing the path to the input
#'   files (e.g., on Windows: 'C:/Data/Blenderdata/', on Linux:
#'   '/home/user/Data/Blenderdata/). Should end with a backslash.
#' @param subjectColumn Logical value. Default is FALSE. Do the single data files contain a column with
#'   the subject number?  If TRUE, the subject number is taken from the single files. If FALSE, the subject number has to be part of the
#'   filename. It should be the last number before the filetype (e.g., '.RData' or '.csv') divided by an underscore from the rest of the 
#'   filename. For example, from the filename 'RawData_Vpn_39.RData' the subject number 39 is generated. 
#' @param outputFilename Name of the output file
#' @param outputDirectory Optional. Path to where the output file should be
#'   saved (e.g., on Windows: 'C:/Data/Blenderdata/output/', on Linux:
#'   '/home/user/Data/Blenderdata/output/). If empty, path of inputDirectory is
#'   beeing used. Should end with a backslash (or slash, respectively).
#' @param verbose If TRUE, the function prints verbose output.
#'   
#' @return Returns data frame and saves Rdata file of concatenated input files.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'   
#' @examples
#' \dontrun{ConcatData()}
#' 
#' @export
ConcatData <- function(dataFileNames, inputDirectory, subjectColumn = FALSE, outputFilename, outputDirectory = "", verbose = FALSE) {
  # Error handling
  if (!is.character(dataFileNames) | !length(dataFileNames) > 1) {
    stop("Argument dataFileNames is not of type character or does not contain two or more filenames!")
  }
  if (!(is.character(inputDirectory))) {
    stop("Argument inputDirectory is not of type character!")
    if (!(dir.exists(inputDirectory))) {
      stop("The input directory is not accessible! Please check the path.")
    }
  }
  if (is.character(outputDirectory)) {
    if ((outputDirectory != "") & (!(dir.exists(outputDirectory)))) {
      stop("The output directory is not accessible! Please check the path.")
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
  
  # Determing file type from file ending (*.csv, *.rda, *.Rdata) (first filename used)
  filetype <- NULL
  if (tolower(strsplit(dataFileNames[1], "\\.")[[1]][2]) == "csv") {
    filetype <- "csv"
  }
  if ((tolower(strsplit(dataFileNames[1], "\\.")[[1]][2]) == "rda") | (tolower(strsplit(dataFileNames[1], "\\.")[[1]][2]) == "rdata")) {
    filetype <- "rda"
  }
  if (!((filetype != "csv") | (filetype != "rda"))) {
    stop("Filetypes not supperted! This function concatenates *.csv and *.rda / *.RData files.")
  }
  
  # Step 1: Determing unique column names and nrows of the files to be concatenated
  if (verbose) {
    writeLines("Step 1: Determing unique column names of the files to be concatenated.")
  }
  
  dataColNames <- NULL
  dataNrows <- NULL
  for (i in 1:length(dataFileNames)) {
    if (verbose) {
      writeLines(paste("Reading columns of file ", dataFileNames[i], " (", i, "/", length(dataFileNames), ")", sep = ""))
    }
    if (filetype == "rda") {
      # loading Rdata-files
      dataName <- load(paste(inputDirectory, dataFileNames[i], sep = ""))
      tempData <- get(dataName)
      rm(list = dataName)  # delete temp data to keep memory usage low
    }
    if (filetype == "csv") {
      # loading csv-files
      tempData <- read.csv2(paste(inputDirectory, dataFileNames[i], sep = ""), header = TRUE, sep = ";", nrows = 1)
    }
    dataColNames <- c(dataColNames, names(tempData))
    dataNrows <- dataNrows + nrow(tempData)
  }
  rm(list = c("tempData"))
  dataColNames <- sort(unique(dataColNames))
  
  if (verbose) {
    writeLines("\nThese are the unique column names of all files to be concatenated. Check, if they are correct.")
    print(dataColNames)
    writeLines("Abort Script? (Press 'y' to abort or any other key to coninue)")
    if (readline(prompt = "? ") == "y") {
      stop("Aborted due to user request.")
    }
  }
  
  # Step 2: Concatenating files
  if (verbose) {
    writeLines("Step 2: Concatenating files.")
  }
  
  # Preallocate empty data frame
  if (subjectColumn) {
    data <- as.data.frame(matrix(data = NA, nrow = dataNrows, ncol = length(dataColNames)))
    colnames(data) <- dataColNames
  } else {
    data <- as.data.frame(matrix(data = NA, nrow = dataNrows, ncol = length(dataColNames) + 1))
    colnames(data) <- c("subject", dataColNames)
  }
  
  # load an concatenate files
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
    
    if (!subjectColumn) {
      filenameParts1 <- strsplit(dataFileNames[i], "\\.")[[1]][1]
      filenameParts2 <- strsplit(filenameParts1, "_")[[1]]
      subject <- as.numeric(filenameParts2[length(filenameParts2)])
      
      tempData <- cbind(subject, tempData)
    } else {
      # Axel: fix me
    }
    # Axel: fix me: http://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
    data <- merge(tempData, data, all = TRUE)
  }
  rm(tempData)
  
  # Step 3: Save output file
  if (verbose) {
    writeLines("Step 3: Saving output file.")
  }
  # Axel: fix me: order by subject
  #dataCen <- dataCen[with(dataCen, order(dataCen[[colNameSubj]], dataCen[[colNameFrames]])), ]
  
  # Axel: fix me: order variables
  # # Reordering columns of input data frame
  #data <- data[c(colNameSubj, colNameFrames, colNameCond, colNames)]
  
  save(data, file = paste(outputDirectory, outputFilename, sep = ""))
  return(data) # Axel: notwendig?
}
