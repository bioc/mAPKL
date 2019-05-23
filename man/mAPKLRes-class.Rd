\name{mAPKLRes-class}
\docType{class}
\alias{mAPKLRes-class}
\alias{mAPKLRes}

\title{Class "mAPKLRes"}
\description{S4 class for storing mAPKL analysis results}

\section{Slots}{
The following slots are defined for \link{mAPKLRes} objects:
\describe{
    \item{\code{rankedIntens}:}{The top N ranked genes along with their
    intensity values}
    \item{\code{exemplTrain}:}{An eSet class object formed with the exemplars
    of the training set}
    \item{\code{exemplTest}:}{An eSet class object formed with the exemplars
    of the validation set if not NULL}
    \item{\code{statistic}:}{A list with the overall results of the "mt.maxT"
    analysis}
    \item{\code{adjp}:}{The adjusted p-values according to the statistical
    analysis}
    \item{\code{pVal}:}{The raw p-values according to the statistical analysis}
    \item{\code{fc}:}{The Fold Change of the exemplars}
    \item{\code{exemplars}:}{The selected "significant" probe ids/genes}
    \item{\code{clusters}:}{The probe ids/genes per cluster}
    }
}

\author{Argiris Sakellariou
    \email{a.sakellariou@gonkhosp.gr}
}
\keyword{classes}
