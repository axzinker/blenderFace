## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("axzinker/blenderFace", built_vignettes = TRUE)
#  library(blenderFace)

## ----echo=FALSE----------------------------------------------------------
library(blenderFace)

## ----eval=FALSE----------------------------------------------------------
#  inputdir <- paste(system.file(package = "blenderFace"),"/extdata/",sep="")
#  outputdir <- paste(system.file(package = "blenderFace"),"/data/",sep="")
#  filenames <- c("Subject_01.csv","Subject_02.csv")
#  
#  # If all files in a directory should be processes, use:
#  # filenames <- list.files(inputdir, pattern = paste("[0-9]",".csv","$",sep=""))
#  
#  concatBlenderFiles(dataFileNames = filenames, inputDirectory = inputdir,
#                     colNameSubj = "", outputFilename = "rawdata.rda",
#                     outputDirectory = outputdir, verbose = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  Step 1: Determine unique column names and number of rows of the files to be concatenated.
#  Reading 2 files:
#  Reading file Subject_01.csv (1/2)
#    Adding 1714 rows to data frame of actually 0 rows.
#  Reading file Subject_02.csv (2/2)
#    Adding 1692 rows to data frame of actually 1714 rows.
#  
#  The final data frame will have 51 columns and 3406 rows.
#  
#  These are the unique column names of all files to be concatenated. Check whether they are correct.
#   [1] "A7_x"         "A7_y"         "A7_z"         "A8_x"         "A8_y"         "A8_z"
#   [7] "BL2_x"        "BL2_y"        "BL2_z"        "BL4_x"        "BL4_y"        "BL4_z"
#  [13] "BL5_x"        "BL5_y"        "BL5_z"        "BL7_x"        "BL7_y"        "BL7_z"
#  [19] "BR2_x"        "BR2_y"        "BR2_z"        "BR4_x"        "BR4_y"        "BR4_z"
#  [25] "BR5_x"        "BR5_y"        "BR5_z"        "BR7_x"        "BR7_y"        "BR7_z"
#  [31] "CL4_x"        "CL4_y"        "CL4_z"        "CL7_x"        "CL7_y"        "CL7_z"
#  [37] "CR4_x"        "CR4_y"        "CR4_z"        "CR7_x"        "CR7_y"        "CR7_z"
#  [43] "DL2_x"        "DL2_y"        "DL2_z"        "DR2_x"        "DR2_y"        "DR2_z"
#  [49] "Frame"        "Stimulustype"
#  Abort Script? (Press 'y' to abort or any other key to continue)
#  ?
#  Step 2: Concatenate files.
#  
#  Preallocating data frame of a 51x3406 matrix.
#  Concatenating file Subject_01.csv (1/2)
#  Concatenating file Subject_02.csv (2/2)
#  Step 3: Savie output file (saving large data files takes some time).

## ----echo=TRUE, eval=TRUE------------------------------------------------
table(rawdata$Stimulustype, rawdata$subject)

## ------------------------------------------------------------------------
# Load the file "Blender_Scalingdata.csv"
scaledata <- read.csv(system.file("extdata", "Blender_Scalingdata.csv", 
                                  package = "blenderFace"), header = TRUE, sep = ",")
# To ensure proper matching, make sure to have the data sorted by subjects
scaledata <- scaledata[with(scaledata, order(scaledata$subject)), ]

## ----eval=TRUE-----------------------------------------------------------
# Get the column names of the scaledata dataframe
names(scaledata)

## ------------------------------------------------------------------------
# Load the file "rawdata"
data(rawdata, package = "blenderFace") # for the package example, please comment out
# load("path/to/your/directory/rawdata.rda") # uncomment and adapt to your work environment
# To ensure proper matching, make sure to have the data sorted by subjects
rawdata <- rawdata[with(rawdata, order(rawdata$subject)), ]

## ----eval=TRUE-----------------------------------------------------------
# Determine the dataframe columns that should be scaled:
names(rawdata)
# -> Frame, Stimulustype and subject should not be scaled -> removed for variable colNames
colNames <- c("A7_x",  "A7_y",  "A7_z",  "A8_x",  "A8_y",  "A8_z",  
              "BL2_x", "BL2_y", "BL2_z", "BL4_x", "BL4_y", "BL4_z", 
              "BL5_x", "BL5_y", "BL5_z", "BL7_x", "BL7_y", "BL7_z",       
              "BR2_x", "BR2_y", "BR2_z", "BR4_x", "BR4_y", "BR4_z", 
              "BR5_x", "BR5_y", "BR5_z", "BR7_x", "BR7_y", "BR7_z", 
              "CL4_x", "CL4_y", "CL4_z", "CL7_x", "CL7_y", "CL7_z",       
              "CR4_x", "CR4_y", "CR4_z", "CR7_x", "CR7_y", "CR7_z", 
              "DL2_x", "DL2_y", "DL2_z", "DR2_x", "DR2_y", "DR2_z")

# To ensure that you will not overwrite existing data, use a new data frame 
# (dataSmm means data scaled in millimeters)
dataSmm <- bu2mm(data = rawdata, colNames = colNames, colNameSubj = "subject", 
                 scaleFactor = scaledata$GlueDotDiameter, rwMeasure = 8, verbose = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  # You have the option to save your data at this stage of the analysis
#  save(dataSmm, file = "path/to/your/directory/dataSmm.rda")

## ------------------------------------------------------------------------
# Load the file "Blender_Scalingdata.csv"
scaledata <- read.csv(system.file("extdata", "Blender_Scalingdata.csv", 
                                  package = "blenderFace"), header = TRUE, sep = ",")
# Make sure to have the data sorted by subjects
scaledata <- scaledata[with(scaledata, order(scaledata$subject)), ]

# Load the file "rawdata"
data(rawdata, package = "blenderFace") # for the package example, please comment out
# load("path/to/your/directory/rawdata.rda") # uncomment and adapt to your work environment
# Make sure to have the data sorted by subjects
rawdata <- rawdata[with(rawdata, order(rawdata$subject)), ]

## ------------------------------------------------------------------------
# Get the column names of the scaledata dataframe
names(scaledata)

# Determine the dataframe columns that should be scaled:
names(rawdata)

# Exclude the columns "Frame," "Stimulustype," "subject," and z-axis columns
colNames <- c("A7_x",  "A7_y",  "A8_x",  "A8_y",  
              "BL2_x", "BL2_y", "BL4_x", "BL4_y",  
              "BL5_x", "BL5_y", "BL7_x", "BL7_y",        
              "BR2_x", "BR2_y", "BR4_x", "BR4_y",  
              "BR5_x", "BR5_y", "BR7_x", "BR7_y",  
              "CL4_x", "CL4_y", "CL7_x", "CL7_y",        
              "CR4_x", "CR4_y", "CR7_x", "CR7_y",  
              "DL2_x", "DL2_y", "DR2_x", "DR2_y")

# To ensure that you will not overwrite existing data, use a new data frame 
# (dataStdF means data of standaradized faces)
dataStdF <- face2stdFace(data = rawdata, colNames = colNames, colNameSubj = "subject", 
                         pupilDist = scaledata$PupilPupilDistance, 
                         leftPMDist = scaledata$LeftPupilLeftMouthcornerDistance)

## ----eval=FALSE----------------------------------------------------------
#  # You have the option to save your data at this stage of the analysis
#  save(dataStdF, file = "path/to/your/directory/dataStdF.rda")

