\name{Classify-class}
\docType{class}
\alias{Classify-class}
\alias{Classify}

\title{Class "Classify"}
\description{S4 class for storing Classify analysis results}

\section{Slots}{
The following slots are defined for \link{Classify} objects:
\describe{
    \item{\code{classL}:}{Number of samples in the data set}
    \item{\code{valClassL}:}{Subset of samples used for leveraged clustering}
    \item{\code{predLbls}:}{Vector containing indices of exemplars}
    \item{\code{AUC}:}{The Area Under the ROC curve as a degree of samples
    discrimination}
    \item{\code{Accuracy}:}{The classification accuracy}
    \item{\code{MCC}:}{The MCC classification meassure}
    \item{\code{Specificity}:}{The degree of true negative's identification}
    \item{\code{Sensitivity}:}{The degree of true positive's identification}
    }
}

\author{Argiris Sakellariou
    \email{a.sakellariou@gonkhosp.gr}
}
\keyword{classes}
