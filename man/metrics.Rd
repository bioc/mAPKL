\name{metrics}
\alias{metrics}

\title{
Computes several clasification metrics
}
\description{
This function calculates several classification related metrics. It uses the
original and the predicted samples' labels to quantify the quality of the
classification process. Those meassures give us a direct outlook of the
selected "genes" and how well discriminate between two phenotypes.
}
\usage{
    metrics(classLbls, predLbls)
}

\arguments{
    \item{classLbls}{
    The initial class labels. %%\code{classLbls}
    }
    \item{predLbls}{
    The predicted class labels. %%\code{predLbls}
    }
}

\value{
    \item{AUC }{The Area Under the ROC curve as a degree of samples
    discrimination}
    \item{Accuracy }{The classification accuracy}
    \item{MCC }{The MCC classification meassure}
    \item{Specificity }{The degree of true negative's identification}
    \item{Sensitivity }{The degree of true positive's identification}

}
\author{
    Argiris Sakellariou
}

\examples{
## Suppose 'val' represent the correct validation set labels
## and 'predictions' the predicted labels according to an SVM model

    val <- c(rep(0,8),rep(1,4))
    predictions <- c(rep(0,6),1,1,rep(1,3),0)
    perfMetrics <- metrics(classLbls=val, predLbls=predictions)

}

\keyword{ methods }
