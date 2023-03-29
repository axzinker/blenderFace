#' Compute the marker movement parameters speed, onset-speed, intensity (amplitude), 
#' irregularites (onset, apex, offset-phases), symmetry)
#' 
#' Output Parameters:
#' 
#' Speed (sp): Mean distance from frame t to t+1
#' 
#' Onset-Speed (onSp): Mean distance from frame t to t+1 only for movement onset
#' phases (see irregularities)
#' 
#' Intensity/Amplitude (int): Mean of the 20 maximal distances from the first frame of 
#' data (= first frame is seen as baseline). Probably use \code{\link{centerCond}} 
#' in advance.
#' 
#' Irregularities (irr): A movement has to be larger than measurement noise and is 
#' defined to be larger than the baseline of .5 (which is .5 mm, if function 
#' \code{\link{bu2mm}} is used beforehand). An onset-phase occurs, if the 
#' distance between t+1 and t+2 is larger than the distance between t and t+1 (previous frame).
#' An apex-phase is defined by the distance between t and t+1 stays whithin
#' the baseline, independent of the direction of movement. An offset-phase occurs, if the distance between t+1 and t+2 
#' is smaller than the distance between t and t+1 (previous frame). Every change in phases is counted. Because the
#' analysed episodes have different numbers of frames per participant, the 
#' relative number of onset, apex, and offset-phases is computed.
#' 
#' Symmetry (sym): Mean correlation (aggregated over axes x,y, and z) of 
#' symmetrical markers. Symmetrical markers are determined by their label, e.g.,
#' if there is a marker label CL7 (for left mouth corner), it is tests if there 
#' is also a marker labeled CR7 (for right mouth corner). If this is true, the 
#' markers CL7 and CR7 are regarded to be symmetrical.
#' 
#' @param data Dataframe containing the data with the columns subject, Frame and
#' marker names labeled according to the blenderFace-method (e.g., CL7_x, CL7_y, 
#' CR7_x, CR7_y; to compute symmetry values). If the dataframe provides values
#' on the z-axis (e.g., CL7_z), the eukledian distances (3D) are computed instead 
#' of the pythagorean distances (2D) to determine marker movement parameters.
#' @param colNames Character vector with marker names of the columns for which
#' the parameters should be computed. A touple or triplet of axes per marker is
#' needed (e.g., CL7_x, CL7_y or CL7_x, CL7_y, CL7_z) to compute parameters.
#' Mirrord markers (e.g., CL7_x, CL7_y and CR7_x, CR7_y) are needed to compute 
#' symmetry values. 
#' @param colNameSubj Character vector with a single value containig the name 
#'   of the subject column of the data data-frame.
#' @param colNameFrames Character vector containing the column name of the 
#' frames column of the data data-frame
#' @param mmCutoff Cutoff for defining a marker movement: It is only regarded to be a movement, if the 
#' distance from t to t+1 is larger than the value of mmCutoff. If NA (default) mmCutoff is computed
#' by the mean + 2SDs of the first 10 frames. However, it can be set to any value, e.g.,
#' to .5, which means a distance of .5 mm if data was scaled to mm via the function \code{\link{bu2mm}}.
#' If movement is smaller than mmCutoff, it is regarded to be an apex-phase or no movement.
#' @param verbose If TRUE, the function prints verbose output. Otherwise not. 
#' Default is FALSE.
#'
#' @import doParallel
#' @import parallel
#' @import foreach
#'    
#' @return returns dataframe containing the marker movement parameters 
#' (speed, onst-speed, intensity, irregularities, symmetry) per subject.
#'   
#' @author Axel Zinkernagel \email{zinkernagel@uni-wuppertal.de}
#'   
#' @examples
#' markers <- c("CL7_x", "CL7_y", "CL7_z", "CR7_x", "CR7_y", "CR7_z")
#' myParams <- mmParameters(dataSmm, colNames = markers,
#'  colNameSubj = "subject",
#'   colNameFrames = "Frame",
#'   verbose = TRUE)
#' myParams
#' 
#' @export
mmParameters <- function(data, colNames, colNameSubj, colNameFrames, mmCutoff = NA, verbose = FALSE){
  
  # Hard coded constants
  foffset = 1 # frame offset, offset to compute differences for. default ist t and t + 2 (= from one frame to the next)
  intensityMeanN <- 20 # Mean of the 20 maximal distances to estimate intensity / amplitude of movement
  mmCutoffFrames <- 10 # Number of frames for which the mmCutoff value should be computed for (mean + 2SD)
  
  # Helper functions ####
  
  ## immediate console output ####
  fcat <- function(..., newline = TRUE) {
    if (newline) cat(...,"\n") 
    else cat(...); flush.console() 
  }  
  
  ## Pythagorean / Eucledian distance between two points in a 2D/3D coordinate system ####
  getDist <- function(Point1, Point2) { # points must be matrices with x/y or x/y/z-values
    Point1 <- as.matrix(Point1)
    Point2 <- as.matrix(Point2)
    dist <- NULL
    
    for (i in 1:nrow(Point1)) {
      dist <- c(dist, sqrt(sum((Point1[i,] - Point2[i,])^2)))
    }
    
    return(as.vector(dist))
  }
  
  ## Compute Distance betwenn two consecutive Frames (per default, offset defined in offset) ####
  # input is a x,y- dataframe, matrix
  distVec <- function(xyzDat, offset = foffset, verboseInternal = FALSE){
    # xyzDat: matrix/dataframe of x and y (and z) coordinates for all frames of a marker (e.g., cbind(CL7_x, CL7_y (, and CL7_z))
    # for which the pyhtagorean / euklydian difference between frame t and t + offset should be computed. Values have to be in
    # columns in the sequence x, y, z.
    
    # Error handling
    if (ncol(xyzDat) < 2 | ncol(xyzDat) > 3 | nrow(xyzDat) < 2) {
      stop("xyzDat does not contain at least 2, or more than 3 columns, or is too short (< 2) for computing a difference!")
    }
    
    xyzDat <- as.matrix(xyzDat)
    
    # generate offset-matrix (diff() does not work here, because we have 2D or 3D data)
    offsetMatr <- matrix(NA, nrow = offset, ncol = ncol(xyzDat))
    
    # Add NA for t1 at the beginning of the vector 
    xyzDat_t1 <- rbind(offsetMatr, xyzDat)
    # Add NA for t2 at the end of the vector
    xyzDat_t2 <- rbind(xyzDat, offsetMatr)
    # Combine the vectors (for controlling purposes)
    xyzDat <- as.data.frame(cbind(xyzDat_t1, xyzDat_t2))
    # name columns
    if (ncol(xyzDat) == 4) {
      names(xyzDat) <- c("xyDat_t1_x", "xyDat_t1_y", "xyDat_t2_x", "xyDat_t2_y")  
    } else {
      names(xyzDat) <- c("xyDat_t1_x", "xyDat_t1_y", "xyDat_t1_z", "xyDat_t2_x", "xyDat_t2_y", "xyDat_t2_z")
    }
    
    # Drop the offset first and offset last row(s) of the dataframe (difference can not be computed because of NAs)
    xyzDat <- xyzDat[-c(1:offset),] # delete the first n (offset) rows
    xyzDat <- xyzDat[c(1:(nrow(xyzDat) - offset)),] # omit the last n (offset) rows
    
    # Compute the difference via getDistance-Function
    dist <- getDist(xyzDat[,1:(ncol(xyzDat)/2)],xyzDat[,(ncol(xyzDat)/2 + 1):ncol(xyzDat)])
    
    if (verboseInternal) {
      n <- 10
      fcat(paste0("Input matrix (first ", n, " lines):"))
      print(head(xyzDat, n = n))
      #print(tail(xyzDat))
      fcat(paste0("Distance vector: (first ", n, " distances):"))
      print(dist[1:n])
      fcat("")
    }
    
    return(dist)
  }
  
  # Input error handling
  
  # Inspect dataframe ####
  subject <- as.numeric(unlist(unique(data[colNameSubj])))
  
  # Test, if colNames are in data dataframe
  if (any(!(c(colNames, colNameSubj, colNameFrames) %in% names(data)))) {
    fcat(paste0("Please check for spelling:"))
    fcat(paste0("colNames not in dataframe: ", paste(setdiff(c(colNames, colNameSubj, colNameFrames), names(data)), collapse = ", ")))
    fcat(paste0("dataframe names not in colNames: ", paste(setdiff(names(data), c(colNames, colNameSubj, colNameFrames)), collapse = ", ")))
    stop("colNames, colNameSubj, or colNameFrames do not match to the column names in the data frame!")
  }
  
  # Test, if colnames are according to blenderFace labeling scheme (= between 4 and 5 characters long)
  if (any((nchar(colNames) > 5) | (nchar(colNames) < 4))) {
    fcat(paste0("To short colNames: ", paste(colNames[nchar(colNames) < 4], collapse = ", ")))
    fcat(paste0("To long colNames: ", paste(colNames[nchar(colNames) > 5], collapse = ", ")))
    stop("colNames seem not to be according to blenderFace labeling scheme: data is not be parsed!")
  }
  
  # Test, if colnames are according to blenderFace labeling scheme (= containing underscore with axis information)
  if (length(grep("._[xyz]$", colNames, perl = TRUE)) != length(colNames)) {
    fcat(paste0("Markerlabel has inccorect axis information (_x, _y, _z): ", paste(grep("._[xyz]$", colNames, perl = TRUE, value = TRUE, invert = TRUE), collapse = ", ")))
  }
  
  colNamesFull <- colNames
  colNames <- strsplit(colNames,"_")
  colDims <- unique(sapply(colNames,function(x) x[2]))
  colNames <- unique(sapply(colNames,function(x) x[1]))
  # Determine mirrored markers (needed for symmetry measures; = search for left 
  # marker names containing an L and look for corresponding marker names containig a R)
  colNamesMirr <- data.frame()
  testMirr <- grep(".L.",colNames, perl = TRUE, value = TRUE)
  for (i in 1:length(testMirr)) {
    if (sub("L","R",testMirr[i]) %in% colNames) {
      colNamesMirr <- rbind(colNamesMirr,cbind(testMirr[i],sub("L","R",testMirr[i])))
    }
  }
  
  # Sort dataframe
  # Axel fix me:  order() produces the warning: In xtfrm.data.frame(x) : cannot xtfrm data frames
  #data <- as.matrix  (data) # this fixes it, but takes a lot of time
  data <- data[order(data[colNameSubj], data[colNameFrames]),]
  #data <- as.data.frame(data)
  
  # Preallocate empty output dataframe
  # columns: subject, frame start, frame stop, per marker: speed, onset-speed, intensity, 
  # irregularites, per mirrored marker: symmetry
  output <- data.frame(matrix(NA, 
                              nrow = length(subject),
                              ncol = (3 + (4 * length(colNames) + (1 * nrow(colNamesMirr))))))
  # label output frame
  labelsOutput <- c("subject", "frameStart", "frameStop")
  labelsOutput <- c(labelsOutput, paste0("sp_",colNames)) # attach speed
  labelsOutput <- c(labelsOutput, paste0("onSp_",colNames)) # attach onset-speed
  labelsOutput <- c(labelsOutput, paste0("int_",colNames)) # attach int
  labelsOutput <- c(labelsOutput, paste0("irr_",colNames)) # attach irregularities
  labelsOutput <- c(labelsOutput, paste0("sym_", gsub("L", "", colNamesMirr[,1])))  # attach irregularities
  names(output) <- labelsOutput

  # # Setting up CPU cluster ####
  # # Axel fix me: parallelized version needs lots of memory and takes forever
  #
  # CPUavail <- parallel::detectCores()
  # cl <- parallel::makeCluster(CPUavail[1] - 1) # use all cores except one, not to overload the computer
  # doParallel::registerDoParallel(cl)
  # 
  # if (verbose) {
  #   fcat(paste0("Starting up CPU cluster: Using ", foreach::getDoParWorkers(), " CPU-cores."))
  #   timestamp0 <- Sys.time()
  # }

  # Compute parameters ####
  #output <- foreach(i = subject, .combine = rbind) %dopar% { 
  for (i in 1:length(subject)) { # start: loop over subjects

    if (verbose) {
      fcat(paste0("Computing Parameters for subject ",subject[i]," (",i,"/",length(subject),"): "), newline = FALSE)
    }
    
    # Select data separately for subjects
    tmpdat <- data[data[, colNameSubj] == subject[i],]
    # remove rows of NAs from tmpdat (e.g. if no markers are tracked) to have values in the first frame row
    tmpdat <- tmpdat[rowSums(is.na(tmpdat[,colNamesFull])) != ncol(tmpdat[,colNamesFull]),]
    
    output[i, "subject"] <- subject[i]
    output[i, "frameStart"] <- min(tmpdat[,colNameFrames])
    output[i, "frameStop"] <- max(tmpdat[,colNameFrames])
        
    ## Distance ####
    # Compute distance matrix (the distance a marker moves from one frame to the next)
    if (verbose) {
      fcat(paste0("distance, "), newline = FALSE)
    }
    distMat <- NULL
    for (j in 1:length(colNames)) {
      distMatCols <- paste0(colNames[j], "_", colDims)
      distMat <- cbind(distMat, distVec(tmpdat[,distMatCols])) # compute the distance for each marker
    }
    distMat <- as.data.frame(distMat)
    names(distMat) <- colNames
    
    ## Speed ####    
    # Speed: Distance from frame t to frame t+1
    if (verbose) {
      fcat(paste0("speed, "), newline = FALSE)
    }
    output[i, paste0("sp_",colNames)] <- colMeans(distMat, na.rm = TRUE)

    ## Intensity ####    
    # Intensity / amplitude: Mean of the 20 maximal distances from the first frame of data.
    if (verbose) {
      fcat(paste0("intensity, "), newline = FALSE)
    }
    intMat <- NULL
    for (j in 1:length(colNames)) {
      intMatCols <- paste0(colNames[j], "_", colDims)
      tmpdat2 <- tmpdat[1,intMatCols]
      tmpdat2 <- tmpdat2[rep(1,nrow(tmpdat)),]
      #rownames(tmpdat2) <- NULL
      # compute the distance for each marker from the first frame (=baseline) and sort it
      intMat <- cbind(intMat, sort(getDist(tmpdat2,tmpdat[,intMatCols]), decreasing = TRUE)[1:intensityMeanN])
    }
    intMat <- as.data.frame(intMat)
    names(intMat) <- colNames
    output[i, paste0("int_",colNames)] <- colMeans(intMat, na.rm = TRUE)

    ## Irregularites #### 
    # (onset, apex, offset-phases)
    if (verbose) {
      fcat(paste0("onset-speed, "), newline = FALSE)
    }
    
    # First: compute irregularities matrix (will also be reused later)
    irrMat <- NULL
    irrCols <- c("onset", "apex", "offset")
    irrColNames <- NULL
    meanMmCutoff <- NA
    for (j in 1:length(colNames)) {
      irrMatCols <- paste0(colNames[j], "_", irrCols)
      irrColNames <- c(irrColNames, irrMatCols)
      irrMat <- cbind(irrMat, matrix(0, nrow = nrow(distMat), byrow = TRUE, ncol = 3)) # fill matrix with 0's
      # Compute mmCutoff
      # minimal marker movement which is regarded to be a movement and not measurement noise
      # Cutoff value is computed from the mean of the first 10 frames + 2SD
      if (is.na(mmCutoff)) {
        tmpdat2 <- distMat[1:mmCutoffFrames ,colNames[j]] 
        mmCutoff <- mean(tmpdat2, na.rm = TRUE) + (2 * sd(tmpdat2, na.rm = TRUE))
        meanMmCutoff <- mean(meanMmCutoff, mmCutoff, na.rm = TRUE)
      }
      irrMat[((distMat[,colNames[j]] > mmCutoff) & (c(NA, diff(distMat[,colNames[j]])) > 0)), ((j * 3) - 2)] <- 1 # onset-phases
      irrMat[(distMat[,colNames[j]] <= mmCutoff), ((j * 3) - 1)] <- 1 # apex-phases
      irrMat[((distMat[,colNames[j]] > mmCutoff) & (c(NA, diff(distMat[,colNames[j]])) < 0)), ((j * 3) - 0)] <- 1 # offset-phase
      # correct for NAs (diff() computes 0 instead of NA)
      irrMat[rowSums(irrMat[, ((j * 3) - 2):((j * 3) - 0)]) == 0]  <- NA 
    }
    irrMat <- as.data.frame(irrMat) 
    names(irrMat) <- irrColNames
    
    # Second: compute onset-speed
    for (j in 1:length(colNames)) {
      output[i, paste0("onSp_",colNames[j])] <- mean(distMat[(irrMat[paste0(colNames[j],"_onset")] == 1),colNames[j]], na.rm = TRUE)
    }
    
    if (verbose) {
      fcat(paste0("irregularities, "), newline = FALSE)
    }
    
    # irregularities: compute changes in phases, summarize and relate to Number of frames
    # (diff computes differences to previous frames; only positive changes (begin of phases) are counted and divided by frames)
    for (j in 1:length(colNames)) {
      onset <- table(diff(irrMat[, paste0(colNames[j],"_onset")]))[3]
      apex <- table(diff(irrMat[, paste0(colNames[j],"_apex")]))[3]
      offset <- table(diff(irrMat[, paste0(colNames[j],"_offset")]))[3]

      if (is.na(onset)) {onset <- 0}
      if (is.na(apex)) {apex <- 0}
      if (is.na(offset)) {offset <- 0}
      
      output[i, paste0("irr_",colNames[j])] <- (onset + apex + offset) / nrow(irrMat)
    }

    ## Symmetry ####
    # Correlation of symmetrical markers per axis; mean computed over per axis correlations
    # Note that, coordinate orignin is at the nosetip -> Movements on the left side of the face
    # have to be reversed on the x-axis to move in the same direction and to correlate positively
    if (verbose) {
      fcat(paste0("symmetry"), newline = FALSE)
    }
    for (j in 1:nrow(colNamesMirr)) {
      corrOut <- NULL
      corrX <- cor((tmpdat[,paste0(colNamesMirr[j,1], "_x")] * -1), tmpdat[,paste0(colNamesMirr[j,2], "_x")], use = "pairwise.complete.obs")
      corrY <- cor(tmpdat[,paste0(colNamesMirr[j,1], "_y")], tmpdat[,paste0(colNamesMirr[j,2], "_y")], use = "pairwise.complete.obs")
      if ("z" %in% colDims) {
        corrZ <- cor(tmpdat[,paste0(colNamesMirr[j,1], "_z")], tmpdat[,paste0(colNamesMirr[j,2], "_z")], use = "pairwise.complete.obs")
        corrOut <- mean(c(corrX, corrY, corrZ), na.rm = TRUE)
      } else {
        corrOut <- mean(c(corrX, corrY), na.rm = TRUE)
      }
      output[i, paste0("sym_",sub("L","",colNamesMirr[j,1]))] <- corrOut
    }
    
    if (verbose) {
      fcat(paste0(""), newline = TRUE) # add a newline to console output
    }
  } # end: loop over subjects
  
  # Count NaNs; this may be a hint that mmCutoff is too strict (e.g., no phases are detected) and give feedback
  numberNaNs <- round((sum(is.nan(unlist(output))) / (nrow(output) * ncol(output))) * 100, digits = 0)
  if (numberNaNs > 10) {
    fcat(paste0("Warning: Your movement parameters contain more than ", numberNaNs, "% NaNs. Thist probably means that the "), newline = FALSE)
    if (!is.na(meanMmCutoff)) {
      mmCutoff <- meanMmCutoff
      fcat("mean marker movement cutoff (mmCutoff) of ", newline = FALSE)
    } else {
      fcat("marker movement cutoff (mmCutoff) of ", newline = FALSE)
    }
    fcat(paste0(mmCutoff, " is too strict."))
    # fcat(paste0("Computing time:", round(timestamp0 - Sys.time(), 2))) # only needed for parallel processing
  }
  
  return(output)
}
