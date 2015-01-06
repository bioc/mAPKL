\name{loadFiles}
\alias{loadFiles}

\title{
Imports gene expression data
}
\description{
This function loads the train set, the class labels files as well as the
test or validation file if any.
Then we may perform normalization and (or) log2 transformation.
}
\usage{
    loadFiles(filesPath, trainFile, labelsFile, validationFile=NULL,
    validationLabels=NULL)
}

\arguments{
    \item{filesPath}{
    The path where the files are stored %%\code{filesPath}
    }
    \item{trainFile}{
    The genes and the relevant intensity values for feature selection analysis.
    The file should be of tab-delimited format %%\code{trainFile}
    }
    \item{labelsFile}{
    The class labels of the samples %%\code{labelsFile}
    }
    \item{validationFile}{
    A further file with genes and intensity values used for validation purposes
    %%\code{validationFile}
    }
    \item{validationLabels}{
    The class labels of the validation samples %%\code{validationLabels}
    }
}
\value{
    An object of Class {\link{DataLD}}
}

\author{
    Argiris Sakellariou
}

\keyword{ IO }
