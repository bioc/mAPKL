\name{netwAttr}
\alias{netwAttr}

\title{
    Calculates network characteristics
}
\description{
    Calculate some basic network characteristics of the top ranked genes
}
\usage{
    netwAttr(mAPKLObj, net="clr")
}

\arguments{
    \item{mAPKLObj}{
    An object of mAPKL class. %%\code{mAPKLObj}
    }    
    \item{net}{
    The network reconstruction method to be employed. The user may select 
    between "clr" (default), "aracne.a" and "aracne.m". %%\code{net}
    }
}
\details{
It calculates some basic network characteristics. Those include the "degree",
the "closeness", the "betweenness", and finally the "transitivity" or else
clustering coefficient. We calculate the weighted values for both local and
global scores. \cr
The three available network reconstruction options are: \cr
clr: Context Likelihood or Relatedness Network \cr
aracne.a: Algorithm for the Reconstruction of Accurate Cellular Networks (additive model) \cr
aracne.m: Algorithm for the Reconstruction of Accurate Cellular Networks (multiplicative model) \cr
}

\value{
Upon successful completion, the function returns an
  \code{\link{NetAttr}} object.
}

\author{
    Argiris Sakellariou
}

\examples{
library(mAPKLData)
data(mAPKLData)
breast <- sampling(Data=mAPKLData, valPercent=40, classLabels="type", seed=135)
normTrainData <- preprocess(breast$trainData)
normTestData <- preprocess(breast$testData)

exprs(breast$trainData) <- normTrainData$clL2.normdata
exprs(breast$testData) <- normTestData$clL2.normdata

out.clL2 <- mAPKL(trObj=breast$trainData, classLabels="type",
valObj=breast$testData, dataType=7)

net.attr <- netwAttr(mAPKLObj=out.clL2)

}

\keyword{ methods }