\name{dataStdF}
\alias{dataStdF}
\docType{data}
\title{
R-data file of data scaled to a standardized face of the two sample subjects.
}
\description{
R-data file of data scaled to a standardized face of the two sample subjects concatenated by the function \link{concatBlenderFiles}. Scaling has been performed with the function \link{face2stdFace} for the `rawdata` dataset.
}
\usage{data("dataStdF")}
\format{
  A data frame with 3406 observations on the following 51 variables.
  \describe{
    \item{\code{AU_01_L_x}}{standardized x-axis movement of Marker at left AU 01}
    \item{\code{AU_01_L_y}}{standardized y-axis movement of Marker at left AU 01}
    \item{\code{AU_01_L_z}}{standardized z-axis movement of Marker at left AU 01}
    \item{\code{AU_01_R_x}}{standardized x-axis movement of Marker at right AU 01}
    \item{\code{AU_01_R_y}}{standardized y-axis movement of Marker at right AU 01}
    \item{\code{AU_01_R_z}}{standardized z-axis movement of Marker at right AU 01}
    \item{\code{AU_02_L_x}}{standardized x-axis movement of Marker at left AU 02}
    \item{\code{AU_02_L_y}}{standardized y-axis movement of Marker at left AU 02}
    \item{\code{AU_02_L_z}}{standardized z-axis movement of Marker at left AU 02}
    \item{\code{AU_02_R_x}}{standardized x-axis movement of Marker at right AU 02}
    \item{\code{AU_02_R_y}}{standardized y-axis movement of Marker at right AU 02}
    \item{\code{AU_02_R_z}}{standardized z-axis movement of Marker at right AU 02}
    \item{\code{AU_06_L_x}}{standardized x-axis movement of Marker at left AU 06}
    \item{\code{AU_06_L_y}}{standardized y-axis movement of Marker at left AU 06}
    \item{\code{AU_06_L_z}}{standardized z-axis movement of Marker at left AU 06}
    \item{\code{AU_06_R_x}}{standardized x-axis movement of Marker at right AU 06}
    \item{\code{AU_06_R_y}}{standardized y-axis movement of Marker at right AU 06}
    \item{\code{AU_06_R_z}}{standardized z-axis movement of Marker at right AU 06}
    \item{\code{AU_08_x}}{standardized x-axis movement of Marker at AU 08}
    \item{\code{AU_08_y}}{standardized y-axis movement of Marker at AU 08}
    \item{\code{AU_08_z}}{standardized z-axis movement of Marker at AU 08}
    \item{\code{AU_09_L_x}}{standardized x-axis movement of Marker at left AU 09}
    \item{\code{AU_09_L_y}}{standardized y-axis movement of Marker at left AU 09}
    \item{\code{AU_09_L_z}}{standardized z-axis movement of Marker at left AU 09}
    \item{\code{AU_09_R_x}}{standardized x-axis movement of Marker at right AU 09}
    \item{\code{AU_09_R_y}}{standardized y-axis movement of Marker at right AU 09}
    \item{\code{AU_09_R_z}}{standardized z-axis movement of Marker at right AU 09}
    \item{\code{AU_10_L_x}}{standardized x-axis movement of Marker at left AU 10}
    \item{\code{AU_10_L_y}}{standardized y-axis movement of Marker at left AU 10}
    \item{\code{AU_10_L_z}}{standardized z-axis movement of Marker at left AU 10}
    \item{\code{AU_10_R_x}}{standardized x-axis movement of Marker at right AU 10}
    \item{\code{AU_10_R_y}}{standardized y-axis movement of Marker at right AU 10}
    \item{\code{AU_10_R_z}}{standardized z-axis movement of Marker at right AU 10}
    \item{\code{AU_11_L_x}}{standardized x-axis movement of Marker at left AU 11}
    \item{\code{AU_11_L_y}}{standardized y-axis movement of Marker at left AU 11}
    \item{\code{AU_11_L_z}}{standardized z-axis movement of Marker at left AU 11}
    \item{\code{AU_11_R_x}}{standardized x-axis movement of Marker at right AU 11}
    \item{\code{AU_11_R_y}}{standardized y-axis movement of Marker at right AU 11}
    \item{\code{AU_11_R_z}}{standardized z-axis movement of Marker at right AU 11}
    \item{\code{AU_12_L_x}}{standardized x-axis movement of Marker at left AU 12}
    \item{\code{AU_12_L_y}}{standardized y-axis movement of Marker at left AU 12}
    \item{\code{AU_12_L_z}}{standardized z-axis movement of Marker at left AU 12}
    \item{\code{AU_12_R_x}}{standardized x-axis movement of Marker at right AU 12}
    \item{\code{AU_12_R_y}}{standardized y-axis movement of Marker at right AU 12}
    \item{\code{AU_12_R_z}}{standardized z-axis movement of Marker at right AU 12}
    \item{\code{AU_16_x}}{standardized x-axis movement of Marker at AU 16}
    \item{\code{AU_16_y}}{standardized y-axis movement of Marker at AU 16}
    \item{\code{AU_16_z}}{standardized z-axis movement of Marker at AU 16}
    \item{\code{Frame}}{Frame number of the video clip per participant}
    \item{\code{Stimulustype}}{Stimulus presented to the participant}
    \item{\code{subject}}{Subject number}
  }
}
\examples{
data(dataStdF)
## maybe str(dataStdF) ; plot(dataStdF) ...
}
\keyword{datasets}
