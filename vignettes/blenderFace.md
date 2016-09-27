---
title: "blenderFace"
author: "Axel Zinkernagel"
date: "2016-09-27"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{blenderFace}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Workflow of Blender-data post-processing with the blenderFace package

For using the blenderFace package it is assumed, that you have tracked facial movements for at least two subjects, following the "Step_by_Step_Instructions.pdf". The output of this procedure is a csv file for each participant, which includes the x,y,z-axis movement of the tracked facial markers. The blenderFace package includes raw data sets of two subjects (in fact, it is one subject recorded and tracked twice). 

First, be sure to have loaded the package:

```r
library(blenderFace)
```

## Concatenating raw data files to one large RData file

To perform the concatenation of files, use the *concatData()* function of the package. If you have followed the "Step-by-Step"-instructions, the subject number is part of the file name. However, the file name ends with "_Step_03" for each participant. Please rename the files, so that the subject number is the last number before the filetype ending (e.g., ".csv"). For example, if you have the file "Subject_39_Step_03.csv", rename it into "Subject_39.csv". The package includes two sample csv files in the `./inst/extdata` directory ("Subject_01.csv","Subject_02.csv"). Although the *concatData()* function does some basic input checks, be aware that this function writes on your hard disk and may change/overwrite/delete files, if messed up input strings are given!

The path specifications are adapted for the package example. Please change the paths to your needs.


```r
inputdir <- paste(system.file(package = "blenderFace"),"/extdata/",sep="")
outputdir <- paste(system.file(package = "blenderFace"),"/data/",sep="")
filenames <- c("Subject_01.csv","Subject_02.csv")

# If all files in a directory should be processes, use:
# filenames <- list.files(inputdir, pattern = paste("[0-9]",".csv","$",sep=""))

concatData(dataFileNames = filenames, inputDirectory = inputdir, subjectColumn = FALSE, outputFilename = "rawdata.rda", outputDirectory = outputdir, verbose = TRUE)
```

The console output of the *concatData()* function shows:


```r
Step 1: Determing unique column names and number of rows of the files to be concatenated.
Reading 2 files:
Reading file Subject_01.csv (1/2)
  Adding 1714 rows to data frame of actually 0 rows.
Reading file Subject_02.csv (2/2)
  Adding 1692 rows to data frame of actually 1714 rows.

The final data frame will have 51 columns and 3406 rows.

These are the unique column names of all files to be concatenated. Check, if they are correct.
 [1] "AU_01_L_x"    "AU_01_L_y"    "AU_01_L_z"    "AU_01_R_x"    "AU_01_R_y"    "AU_01_R_z"    "AU_02_L_x"    "AU_02_L_y"    "AU_02_L_z"   
[10] "AU_02_R_x"    "AU_02_R_y"    "AU_02_R_z"    "AU_06_L_x"    "AU_06_L_y"    "AU_06_L_z"    "AU_06_R_x"    "AU_06_R_y"    "AU_06_R_z"   
[19] "AU_08_x"      "AU_08_y"      "AU_08_z"      "AU_09_L_x"    "AU_09_L_y"    "AU_09_L_z"    "AU_09_R_x"    "AU_09_R_y"    "AU_09_R_z"   
[28] "AU_10_L_x"    "AU_10_L_y"    "AU_10_L_z"    "AU_10_R_x"    "AU_10_R_y"    "AU_10_R_z"    "AU_11_L_x"    "AU_11_L_y"    "AU_11_L_z"   
[37] "AU_11_R_x"    "AU_11_R_y"    "AU_11_R_z"    "AU_12_L_x"    "AU_12_L_y"    "AU_12_L_z"    "AU_12_R_x"    "AU_12_R_y"    "AU_12_R_z"   
[46] "AU_16_x"      "AU_16_y"      "AU_16_z"      "Frame"        "Stimulustype"
Abort Script? (Press 'y' to abort, or any other key to coninue)
? 
Step 2: Concatenating files.

Preallocating data frame of a 51x3406 matrix.
Concatenating file Subject_01.csv (1/2)
Concatenating file Subject_02.csv (2/2)
Step 3: Saving output file (it takes time to save large files).
```

As main output of this function a file with the filename given in `outputFilename` is saved in the directory given in as `outputDirectory`. The data frame stored in this file contains the data of all concatenated subjects. This output file of the two sample subjects is also attached to this package.

## Scale Blender units to real world measures

The next step in Blender data post-processing is to scale the Blender units (BU) to real world measures, e.g., into mm. In principle, the rescaling is done by rule of proportion:
$$latex
\begin{equation*}
  \frac{\mbox{Diameter in Blender units}}{\mbox{Diameter in millimeter}} =
  \mbox{Factor to multiply BU with, to obtain mm}
\end{equation*}
$$

In the sample video "Subject_01.mp4" glue-dots with 8 mm diameter were used. Additionally, the glue-dot diameter measurement in BU is needed. If you have followed the "Step-by-Step"-instructions, you have measured and saved the glue-dot diameters in BU in a file called "Blender_Scalingdata.csv". For the two example subjects, the file "Blender_Scalingdata.csv" is included in the package.

The path specifications are adapted for the package example. Please change the paths to your needs.


```r
scaledata <- read.csv(system.file("extdata", "Blender_Scalingdata.csv", package = "blenderFace"), header = TRUE, sep =",")
# Be sure to have the table sorted by subjects
scaledata <- scaledata[with(scaledata, order(scaledata$subject)), ]

data(rawdata, package="blenderFace")
names(rawdata)
```

```
## Error in eval(expr, envir, enclos): Objekt 'rawdata' nicht gefunden
```


```r
colNames <- c("AU_01_L_x", "AU_01_L_y", "AU_01_L_z", "AU_01_R_x", "AU_01_R_y", "AU_01_R_z", "AU_02_L_x", "AU_02_L_y", "AU_02_L_z", "AU_02_R_x", "AU_02_R_y", 
              "AU_02_R_z", "AU_06_L_x", "AU_06_L_y", "AU_06_L_z", "AU_06_R_x", "AU_06_R_y", "AU_06_R_z", "AU_08_x", "AU_08_y", "AU_08_z", "AU_09_L_x",  
              "AU_09_L_y", "AU_09_L_z", "AU_09_R_x", "AU_09_R_y", "AU_09_R_z", "AU_10_L_x", "AU_10_L_y", "AU_10_L_z", "AU_10_R_x", "AU_10_R_y", "AU_10_R_z",  
              "AU_12_L_x", "AU_12_L_y", "AU_12_L_z", "AU_12_R_x", "AU_12_R_y", "AU_12_R_z", "AU_16_x", "AU_16_y", "AU_16_z")

load(paste(outputDirectory,"BdataRaw.Rdata",sep=""))

# Order columns of data frame accordingly
data <- data[c("subject", "Frame", "stimulustype", colNames)]

# Read table with scaling data per subject to scale blender units to real world measures
scaleTable <- read.csv(paste(inputDirectory,"Hilfsdaten.csv",sep=""),header = TRUE, sep = ";")
# Sort table by subjects
scaleTable <- scaleTable[with(scaleTable, order(scaleTable$subject)), ]

BdataScaledB <- scaleBlenderData(data, colNames, colNameSubj="subject", scaleTable$Skalierung_Markerpunkt, verbose=TRUE)
save(BdataScaledB, file=(paste(outputDirectory,"BdataScaledB.Rdata",sep="")))
```
















*Axel* _*fix me*_: Im Beispielvideo falsche Markerbennung (AU_08_R) korrigieren!







The figure sizes have been customised so that you can easily put two images side-by-side. 


```r
plot(1:10)
plot(10:1)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-2.png)

You can enable figure captions by `fig_caption: yes` in YAML:

    output:
      rmarkdown::html_vignette:
        fig_caption: yes

Then you can use the chunk option `fig.cap = "Your figure caption."` in **knitr**.

You can write math expressions, e.g. $Y = X\beta + \epsilon$, footnotes^[A footnote here.], and tables, e.g. using `knitr::kable()`.


|                  |  mpg| cyl|  disp|  hp| drat|    wt|  qsec| vs| am| gear| carb|
|:-----------------|----:|---:|-----:|---:|----:|-----:|-----:|--:|--:|----:|----:|
|Mazda RX4         | 21.0|   6| 160.0| 110| 3.90| 2.620| 16.46|  0|  1|    4|    4|
|Mazda RX4 Wag     | 21.0|   6| 160.0| 110| 3.90| 2.875| 17.02|  0|  1|    4|    4|
|Datsun 710        | 22.8|   4| 108.0|  93| 3.85| 2.320| 18.61|  1|  1|    4|    1|
|Hornet 4 Drive    | 21.4|   6| 258.0| 110| 3.08| 3.215| 19.44|  1|  0|    3|    1|
|Hornet Sportabout | 18.7|   8| 360.0| 175| 3.15| 3.440| 17.02|  0|  0|    3|    2|
|Valiant           | 18.1|   6| 225.0| 105| 2.76| 3.460| 20.22|  1|  0|    3|    1|
|Duster 360        | 14.3|   8| 360.0| 245| 3.21| 3.570| 15.84|  0|  0|    3|    4|
|Merc 240D         | 24.4|   4| 146.7|  62| 3.69| 3.190| 20.00|  1|  0|    4|    2|
|Merc 230          | 22.8|   4| 140.8|  95| 3.92| 3.150| 22.90|  1|  0|    4|    2|
|Merc 280          | 19.2|   6| 167.6| 123| 3.92| 3.440| 18.30|  1|  0|    4|    4|

Also a quote using `>`:

> "He who gives up [code] safety for [code] speed deserves neither."
([via](https://twitter.com/hadleywickham/status/504368538874703872))
