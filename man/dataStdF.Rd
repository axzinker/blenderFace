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
    \item{\code{BL2_x}}{standardized x-axis movement of Marker at BL2}
    \item{\code{BL2_y}}{standardized y-axis movement of Marker at BL2}
    \item{\code{BL2_z}}{standardized z-axis movement of Marker at BL2}
    \item{\code{BR2_x}}{standardized x-axis movement of Marker at BR2}
    \item{\code{BR2_y}}{standardized y-axis movement of Marker at BR2}
    \item{\code{BR2_z}}{standardized z-axis movement of Marker at BR2}
    \item{\code{DL2_x}}{standardized x-axis movement of Marker at DL2}
    \item{\code{DL2_y}}{standardized y-axis movement of Marker at DL2}
    \item{\code{DL2_z}}{standardized z-axis movement of Marker at DL2}
    \item{\code{DR2_x}}{standardized x-axis movement of Marker at DR2}
    \item{\code{DR2_y}}{standardized y-axis movement of Marker at DR2}
    \item{\code{DR2_z}}{standardized z-axis movement of Marker at DR2}
    \item{\code{CL4_x}}{standardized x-axis movement of Marker at CL4}
    \item{\code{CL4_y}}{standardized y-axis movement of Marker at CL4}
    \item{\code{CL4_z}}{standardized z-axis movement of Marker at CL4}
    \item{\code{CR4_x}}{standardized x-axis movement of Marker at CR4}
    \item{\code{CR4_y}}{standardized y-axis movement of Marker at CR4}
    \item{\code{CR4_z}}{standardized z-axis movement of Marker at CR4}
    \item{\code{A7_x}}{standardized x-axis movement of Marker at A7}
    \item{\code{A7_y}}{standardized y-axis movement of Marker at A7}
    \item{\code{A7_z}}{standardized z-axis movement of Marker at A7}
    \item{\code{BL4_x}}{standardized x-axis movement of Marker at BL4}
    \item{\code{BL4_y}}{standardized y-axis movement of Marker at BL4}
    \item{\code{BL4_z}}{standardized z-axis movement of Marker at BL4}
    \item{\code{BR4_x}}{standardized x-axis movement of Marker at BR4}
    \item{\code{BR4_y}}{standardized y-axis movement of Marker at BR4}
    \item{\code{BR4_z}}{standardized z-axis movement of Marker at BR4}
    \item{\code{BL7_x}}{standardized x-axis movement of Marker at BL7}
    \item{\code{BL7_y}}{standardized y-axis movement of Marker at BL7}
    \item{\code{BL7_z}}{standardized z-axis movement of Marker at BL7}
    \item{\code{BR7_x}}{standardized x-axis movement of Marker at BR7}
    \item{\code{BR7_y}}{standardized y-axis movement of Marker at BR7}
    \item{\code{BR7_z}}{standardized z-axis movement of Marker at BR7}
    \item{\code{BL5_x}}{standardized x-axis movement of Marker at BL5}
    \item{\code{BL5_y}}{standardized y-axis movement of Marker at BL5}
    \item{\code{BL5_z}}{standardized z-axis movement of Marker at BL5}
    \item{\code{BR5_x}}{standardized x-axis movement of Marker at BR5}
    \item{\code{BR5_y}}{standardized y-axis movement of Marker at BR5}
    \item{\code{BR5_z}}{standardized z-axis movement of Marker at BR5}
    \item{\code{CL7_x}}{standardized x-axis movement of Marker at CL7}
    \item{\code{CL7_y}}{standardized y-axis movement of Marker at CL7}
    \item{\code{CL7_z}}{standardized z-axis movement of Marker at CL7}
    \item{\code{CR7_x}}{standardized x-axis movement of Marker at CR7}
    \item{\code{CR7_y}}{standardized y-axis movement of Marker at CR7}
    \item{\code{CR7_z}}{standardized z-axis movement of Marker at CR7}
    \item{\code{A8_x}}{standardized x-axis movement of Marker at A8}
    \item{\code{A8_y}}{standardized y-axis movement of Marker at A8}
    \item{\code{A8_z}}{standardized z-axis movement of Marker at A8}
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
