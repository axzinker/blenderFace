\name{dataStdFCen}
\alias{dataStdFCen}
\docType{data}
\title{
R-data file of data scaled to a standardized and centered per stimulus condition face of the two sample subjects.
}
\description{
R-data file of data scaled to a standardized and centered per stimulus condition face of the two sample subjects concatenated by the function \code{\link{concatBlenderFiles}}. Scaling has been performed with the function \code{\link{face2stdFace}} for the `rawdata` dataset. Additionally, the data has been centered for each stimulus condition onset via the function \code{\link{centerCond}}.
}
\usage{data("dataStdFCen")}
\format{
  A data frame with 3406 observations on the following 51 variables.
  \describe{
    \item{\code{BL2_x}}{standardized and centered per stimulus condition x-axis movement of Marker at BL2}
    \item{\code{BL2_y}}{standardized and centered per stimulus condition y-axis movement of Marker at BL2}
    \item{\code{BR2_x}}{standardized and centered per stimulus condition x-axis movement of Marker at BR2}
    \item{\code{BR2_y}}{standardized and centered per stimulus condition y-axis movement of Marker at BR2}
    \item{\code{DL2_x}}{standardized and centered per stimulus condition x-axis movement of Marker at DL2}
    \item{\code{DL2_y}}{standardized and centered per stimulus condition y-axis movement of Marker at DL2}
    \item{\code{DR2_x}}{standardized and centered per stimulus condition x-axis movement of Marker at DR2}
    \item{\code{DR2_y}}{standardized and centered per stimulus condition y-axis movement of Marker at DR2}
    \item{\code{CL4_x}}{standardized and centered per stimulus condition x-axis movement of Marker at CL4}
    \item{\code{CL4_y}}{standardized and centered per stimulus condition y-axis movement of Marker at CL4}
    \item{\code{CR4_x}}{standardized and centered per stimulus condition x-axis movement of Marker at CR4}
    \item{\code{CR4_y}}{standardized and centered per stimulus condition y-axis movement of Marker at CR4}
    \item{\code{A7_x}}{standardized and centered per stimulus condition x-axis movement of Marker at A7}
    \item{\code{A7_y}}{standardized and centered per stimulus condition y-axis movement of Marker at A7}
    \item{\code{BL4_x}}{standardized and centered per stimulus condition x-axis movement of Marker at BL4}
    \item{\code{BL4_y}}{standardized and centered per stimulus condition y-axis movement of Marker at BL4}
    \item{\code{BR4_x}}{standardized and centered per stimulus condition x-axis movement of Marker at BR4}
    \item{\code{BR4_y}}{standardized and centered per stimulus condition y-axis movement of Marker at BR4}
    \item{\code{BL7_x}}{standardized and centered per stimulus condition x-axis movement of Marker at BL7}
    \item{\code{BL7_y}}{standardized and centered per stimulus condition y-axis movement of Marker at BL7}
    \item{\code{BR7_x}}{standardized and centered per stimulus condition x-axis movement of Marker at BR7}
    \item{\code{BR7_y}}{standardized and centered per stimulus condition y-axis movement of Marker at BR7}
    \item{\code{BL5_x}}{standardized and centered per stimulus condition x-axis movement of Marker at BL5}
    \item{\code{BL5_y}}{standardized and centered per stimulus condition y-axis movement of Marker at BL5}
    \item{\code{BR5_x}}{standardized and centered per stimulus condition x-axis movement of Marker at BR5}
    \item{\code{BR5_y}}{standardized and centered per stimulus condition y-axis movement of Marker at BR5}
    \item{\code{CL7_x}}{standardized and centered per stimulus condition x-axis movement of Marker at CL7}
    \item{\code{CL7_y}}{standardized and centered per stimulus condition y-axis movement of Marker at CL7}
    \item{\code{CR7_x}}{standardized and centered per stimulus condition x-axis movement of Marker at CR7}
    \item{\code{CR7_y}}{standardized and centered per stimulus condition y-axis movement of Marker at CR7}
    \item{\code{A8_x}}{standardized and centered per stimulus condition x-axis movement of Marker at A8}
    \item{\code{A8_y}}{standardized and centered per stimulus condition y-axis movement of Marker at A8}
    \item{\code{Frame}}{Frame number of the video clip per participant}
    \item{\code{Stimulustype}}{Stimulus presented to the participant}
    \item{\code{subject}}{Subject number}
  }
}
\examples{
data(dataStdFCen)
## maybe str(dataStdF) ; plot(dataStdF) ...
}
\keyword{datasets}
