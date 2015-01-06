\name{NetAttr-class}
\docType{class}
\alias{NetAttr-class}
\alias{NetAttr}

\title{Class "NetAttr"}
\description{
S4 class for storing some network characteristics of the top ranked genes
}

\section{Slots}{
The following slots are defined for \link{NetAttr} objects:
    \describe{
    \item{\code{edgelist}:}{The Node1 & Node2 & Weight edgelist matrix}
    \item{\code{degree}:}{The Local, Global and their Weighted values of the
    "degree" characteristic}
    \item{\code{closeness}:}{The Weighted values of the Local and Global
    "closeness" characteristic}
    \item{\code{betweenness}:}{The Weighted values of the Local and Global
    "betweenness" characteristic}
    \item{\code{transitivity}:}{The Weighted values of the Local and Global
    "transitivity" characteristic}
    }
}

\author{Argiris Sakellariou
    \email{a.sakellariou@gonkhosp.gr}
}
\keyword{classes}
