## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("axzinker/blenderFace", built_vignettes = TRUE)
#  library(blenderFace)

## ----echo=FALSE---------------------------------------------------------------
library(blenderFace)

## ----eval=FALSE---------------------------------------------------------------
#  inputdir <- paste(system.file(package = "blenderFace"),"/extdata/",sep="")
#  outputdir <- paste(system.file(package = "blenderFace"),"/data/",sep="")
#  filenames <- c("Subject_01.csv","Subject_02.csv")
#  
#  # If all files in a directory should be processes, use:
#  # filenames <- list.files(inputdir, pattern = paste("[0-9]",".csv","$",sep=""))
#  
#  rawdata <- concatBlenderFiles(dataFileNames = filenames, inputDirectory = inputdir,
#                     colNameSubj = "", verbose = TRUE)

## ----eval=FALSE---------------------------------------------------------------
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
#  Step 3: Returning output dataframe (may take some time if input data is large).

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
table(rawdata$Stimulustype, rawdata$subject)

## -----------------------------------------------------------------------------
# Load the file "Blender_Scalingdata.csv"
scaledata <- read.csv(system.file("extdata", "Blender_Scalingdata.csv", 
                                  package = "blenderFace"), header = TRUE, sep = ",")
# To ensure proper matching, make sure to have the data sorted by subjects
scaledata <- scaledata[with(scaledata, order(scaledata$subject)), ]

## ----eval=TRUE----------------------------------------------------------------
# Get the column names of the scaledata dataframe
names(scaledata)

## -----------------------------------------------------------------------------
# Load the file "rawdata"
data(rawdata, package = "blenderFace") # for the package example, please comment out
# load("path/to/your/directory/rawdata.rda") # uncomment and adapt to your work environment
# To ensure proper matching, make sure to have the data sorted by subjects
rawdata <- rawdata[with(rawdata, order(rawdata$subject)), ]

## ----eval=TRUE----------------------------------------------------------------
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

## ----eval=FALSE---------------------------------------------------------------
#  # You have the option to save your data at this stage of the analysis
#  save(dataSmm, file = "path/to/your/directory/dataSmm.rda")

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
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

## ----eval=FALSE---------------------------------------------------------------
#  # You have the option to save your data at this stage of the analysis
#  save(dataStdF, file = "path/to/your/directory/dataStdF.rda")

## -----------------------------------------------------------------------------
plotOutliers(subset(dataSmm, subset = (dataSmm$subject == 1)), colNameFrames = "Frame", 
             colNameData = c("DR2_x","DR2_y"), colNameCond = "Stimulustype")

## -----------------------------------------------------------------------------
outlierTest <- subset(dataSmm, subset = (dataSmm$subject == 1))
outlierTest[995,"DR2_x"] <- outlierTest[995,"DR2_x"] + 2

## ----eval=FALSE---------------------------------------------------------------
#  > plotOutliers(outlierTest, colNameFrames = "Frame", colNameData = c("DR2_x","DR2_y"),
#                 colNameCond = "Stimulustype")
#  1 possible outlier(s) found:
#             Dist     Frames
#  [1,]   2.741625 996.000000
#  Plot each outlier? ('y', 'n', or 'c' to cancel)
#  (y,n,c)?

## ---- eval=FALSE--------------------------------------------------------------
#  plotOutliers(outlierTest, colNameFrames = "Frame", colNameData = c("DR2_x","DR2_y"),
#               colNameCond = "Stimulustype", title = "Subject 1, DR2")

## ---- eval=FALSE--------------------------------------------------------------
#  for (i in 1:length(subjects)) { # loop over participants
#    for (j in 1:(length(MarkerNames)/2)) { # loop over marker-pairs (x/y)
#      plotOutliers(subset(outlierTest, subset = (outlierTest$subject == subjects[i])),
#                   colNameFrames = "Frame",
#                   colNameData = c(MarkerNames[j*2-1],MarkerNames[j*2]),
#                   title = paste0("Outl_Subj_",subjects[i],"_",
#                                  MarkerNames[j*2-1],"_",MarkerNames[j*2]),
#                   savePlots = TRUE)
#    }
#  }

## -----------------------------------------------------------------------------
colNames <- c("A7_x",  "A7_y",  "A8_x",  "A8_y",  
              "BL2_x", "BL2_y", "BL4_x", "BL4_y",  
              "BL5_x", "BL5_y", "BL7_x", "BL7_y",        
              "BR2_x", "BR2_y", "BR4_x", "BR4_y",  
              "BR5_x", "BR5_y", "BR7_x", "BR7_y",  
              "CL4_x", "CL4_y", "CL7_x", "CL7_y",        
              "CR4_x", "CR4_y", "CR7_x", "CR7_y",  
              "DL2_x", "DL2_y", "DR2_x", "DR2_y")
# To ensure that you will not overwrite existing data, use a new data frame 
# (dataStdFCen means data of standardized faces, centered)
# (only 1 Core is used to follow CRAN and Travis prerequisites)
dataStdFCen <- centerCond(dataStdF, colNames = colNames, colNameSubj = "subject", 
                          colNameFrames = "Frame", colNameCond = "Stimulustype", 
                          maxCPUcores = 1, verbose = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  # You have the option to save your data at this stage of the  analysis
#  save(dataStdFCen, file = "path/to/your/directory/dataStdFCen.rda")

## ---- fig.show='hold', fig.height=7, fig.width=7------------------------------
# Select data for Subject 2
# In addition, omit untracked frames at the start and the end of the video clip
data_Subj2 <- subset(dataSmm, subset = ((dataSmm$subject == 2) & 
                                          (dataSmm$Frame >= 690)& (dataSmm$Frame <= 1610)))
plotXYmmpf(colFrames = data_Subj2$Frame, colX = data_Subj2$BR4_x, 
           colY = data_Subj2$BR4_y, colCond = data_Subj2$Stimulustype, 
           center = FALSE, title = "Subject 2, BR4")

## ---- fig.show='hold', fig.height=7, fig.width=7------------------------------
# Plot right marker of BR4 (Centered)
plotXYmmpf(colFrames = data_Subj2$Frame, colX = data_Subj2$BR4_x, 
           colY = data_Subj2$BR4_y, center = TRUE, title = "Subject 1, 
           BR4 (red, orange), BL4 (blue, green)")
# Add left marker of BR4
plotXYmmpf(colFrames = data_Subj2$Frame, colX = data_Subj2$BL4_x, 
           colY = data_Subj2$BL4_y, center = TRUE, color = c("blue", "green"), 
           overplot = TRUE)

## ---- fig.show='hold', fig.height=7, fig.width=7------------------------------
data_Subj2Cen <- subset(dataStdFCen, subset = ((dataStdFCen$subject == 2) & 
                                                 (dataStdFCen$Frame >= 690) & 
                                                 (dataStdFCen$Frame <= 1610)))
plotXYmmpf(colFrames = data_Subj2Cen$Frame, colX = data_Subj2Cen$BR4_x, 
           colY = data_Subj2Cen$BR4_y, colCond = data_Subj2Cen$Stimulustype, 
           center = FALSE, title = "Subject 2, BR4")

## ---- fig.show='hold', fig.height=7, fig.width=7------------------------------
colNames <- c("A7_x",  "A7_y",  "A8_x",  "A8_y",  
              "BL2_x", "BL2_y", "BL4_x", "BL4_y",  
              "BL5_x", "BL5_y", "BL7_x", "BL7_y",        
              "BR2_x", "BR2_y", "BR4_x", "BR4_y",  
              "BR5_x", "BR5_y", "BR7_x", "BR7_y",  
              "CL4_x", "CL4_y", "CL7_x", "CL7_y",        
              "CR4_x", "CR4_y", "CR7_x", "CR7_y",  
              "DL2_x", "DL2_y", "DR2_x", "DR2_y")

# Select data for plotting (selecting stimulus type and omit z-axis)
data_Subj_happy <- subset(dataStdFCen, subset = (dataStdFCen$Stimulustype == "posed_happy"), 
                          select = c("subject",colNames))
data_Subj_disgust <- subset(dataStdFCen, subset = (dataStdFCen$Stimulustype == "posed_disgust"), 
                            select = c("subject",colNames))

# Define the positions for the markers for the standardized face of x (-1,1) 
# and y (-1,1) size as named list
dataPos <- list(BL2 = c(-.3,.7), BR2 = c(.3,.7), 
                DL2 = c(-.7,.7), DR2 = c(.7,.7), 
                BL4 = c(-.2,.2), BR4 = c(.2,.2), 
                CL4 = c(-.5,.2), CR4 = c(.5,.2), 
                BL5 = c(-.2,-.1), BR5 = c(.2,-.1), 
                BL7 = c(-.2,-.6), BR7 = c(.2,-.6), 
                CL7 = c(-.3,-.7), CR7 = c(.3,-.7), 
                A7 = c(0,-.55), 
                A8 = c(0,-.8)) 

# For debugging purposes the marker names and start positions may also be plotted
plotXhead(data = data_Subj_happy[-1], dataPos = dataPos, 
          title = "All Subjects, happy", plotDataPos = TRUE)

## ---- fig.show='hold', fig.height=7, fig.width=7------------------------------
plotXhead(data = data_Subj_happy[-1], dataPos = dataPos, 
          title = "All Subjects, happy (black) vs. disgust (red)")
plotXhead(data = data_Subj_disgust[-1], dataPos = dataPos, overplot = TRUE, color = "red")

## ---- fig.show='hold', , fig.height=3.4, fig.width=3.4------------------------
plotIndmm(data = data_Subj_happy, colNames = c("CL7_x", "CL7_y"), 
          colNameSubj = "subject", title = "Posed Happy CL7")
plotIndmm(data = data_Subj_happy, colNames = c("CR7_x", "CR7_y"), 
          colNameSubj = "subject", title = "Posed Happy CR7",xlim = c(-.05,.05), 
          ylim = c(-.05,.05), verbose = TRUE)

## ---- fig.show='hold', fig.height=7, fig.width=7------------------------------
plotMmpCond(data = dataStdFCen, colNames = c("CL7_x", "CL7_y"), 
            colNameCond = "Stimulustype", title = "CL7")

## -----------------------------------------------------------------------------
# Data preparation
data_Subj_happy <- subset(dataSmm, subset = dataSmm["Stimulustype"] == "posed_happy", 
                          select = c("subject",colNames))
data_Subj_disgust <- subset(dataSmm, subset = dataSmm["Stimulustype"] == "posed_disgust", 
                            select = c("subject",colNames))

angleDist(data_Subj_happy, colNames = c("CL7_x", "CL7_y"), 
          colNameSubj = "subject", rndDig = 3)
angleDist(data_Subj_happy, colNames = c("CR7_x", "CR7_y"), 
          colNameSubj = "subject", rndDig = 3, verbose = TRUE)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(angleDist(data_Subj_disgust, colNames = c("BL4_x", "BL4_y"), 
                       colNameSubj = "subject"))

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(angleDist(data_Subj_happy, colNames = c("BL4_x", "BL4_y"), 
                       colNameSubj = "subject"))

## -----------------------------------------------------------------------------
marker3D <- c("A7_x", "A7_y", "A7_z", "A8_x", "A8_y", "A8_z", 
             "BL2_x", "BL2_y", "BL2_z", "BL4_x", "BL4_y", "BL4_z",
             "BL5_x", "BL5_y", "BL5_z", "BL7_x", "BL7_y", "BL7_z",
             "BR2_x", "BR2_y", "BR2_z", "BR4_x", "BR4_y", "BR4_z",
             "BR5_x", "BR5_y", "BR5_z", "BR7_x", "BR7_y", "BR7_z",
             "CL4_x", "CL4_y", "CL4_z", "CL7_x", "CL7_y", "CL7_z",
             "CR4_x", "CR4_y", "CR4_z", "CR7_x", "CR7_y", "CR7_z", 
             "DL2_x", "DL2_y", "DL2_z", "DR2_x", "DR2_y", "DR2_z") 

movementParameters <- mmParameters(dataSmm, colNames = marker3D, 
                                   colNameSubj = "subject", 
                                   colNameFrames = "Frame", 
                                   verbose = TRUE)

## -----------------------------------------------------------------------------
# Print table
movementParameters

