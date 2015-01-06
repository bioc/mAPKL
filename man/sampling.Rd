\name{sampling}
\alias{sampling}

\title{ Splits a dataset to a train and a test sets of a user defined
    percentage
}
\description{
This function takes as input a dataset and splits it into a train and a test
set based to a user defined percentage.
}
\usage{
    sampling(Data,valPercent,classLabels,seed)
}

\arguments{
    \item{Data}{
    The input dataset to be split as an eSet object. %%\code{Data}
    }
    \item{valPercent}{
    The percentage of the input dataset used for validation purposes e.g. 40.
    %%\code{valPercent}
    }
    \item{classLabels}{
    The varLabels name in the eSet object where the class labels are stored e.g
    "type". %%\code{classLabels}
    }
    \item{seed}{
    Setting the seed number for reproducible sampling. The default value is
    1. %%\code{seed}
    }
}

\value{
    \item{trainData }{The data used for training as an eSet object}
    \item{testData }{The data used for validation as an eSet object}
}

\author{
    Argiris Sakellariou
}

\examples{
library(mAPKLData)
data(mAPKLData)
breast <- sampling(Data=mAPKLData, valPercent=40, classLabels="type", seed=135)
}

\keyword{ datagen }
