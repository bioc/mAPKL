\name{preprocess}
\alias{preprocess}

\title{
Performs normalization and/or log2 transformation
}
\description{
This function performs normalization and/or log2 transformation on gene
expression data.
}
\usage{
    preprocess(exprsObj,log2=TRUE, norm="ALL", destname=NULL)
}

\arguments{
    \item{exprsObj}{An eSet object where its assay data will be normalized
    %%\code{exprsObj}
    }
    \item{log2}{Performs logarithmic transformation of base 2 prior to any
    normalzation. The default value is TRUE %%\code{log2}
    }
    \item{norm}{The user may define a specific normalization method rather than
    "ALL" which is the default case. The available abbreviations are described
    in the details section %%\code{norm}
    }
    \item{destname}{Here we define the destination path and the name of the jpeg
    file with the density plots. The default path is the working directory
    %%\code{destname}
    }
}
\details{
    The available normalization methods are: \cr
    Mean-centering normalization  "mc" \cr
    z-score normalization  "z" \cr
    Quantile normalization  "q" \cr
    Cyclic loess normalization "cl" \cr
    Mean-centering normalization and log2 transformation  "mcL2" \cr
    z-score normalization and log2 transformation  "zL2" \cr
    Quantile normalization and log2 transformation "qL2" \cr
    Cyclic loess normalization and log2 transformation "clL2" \cr
}
\value{
    \item{rawdata}{The initial gene expression values}
    \item{mc.normdata }{The values after 'mean-centering' normalization}
    \item{z.normdata }{The values after 'z-score' normalization}
    \item{q.normdata }{The values after 'quantile' normalization}
    \item{cl.normdata }{The values after 'cyclic loess' normalization}
    \item{mcL2.normdata }{The values after 'mean-centering' normalization and
    log2}
    \item{zL2.normdata }{The values after 'z-score' normalization and log2}
    \item{qL2.normdata }{The values after 'quantile' normalization and log2}
    \item{clL2.normdata }{The values after 'cyclic loess' normalization and
    log2}
}
\author{
    Argiris Sakellariou
}

\examples{

library(mAPKLData)
data(mAPKLData)
varLabels(mAPKLData)
breast <- sampling(Data=mAPKLData, valPercent=40, classLabels="type", seed=135)
normTrainData <- preprocess(exprsObj=breast$trainData)

}
\keyword{ methods }
