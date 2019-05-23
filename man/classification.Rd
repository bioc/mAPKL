\name{classification}
\alias{classification}

\title{
    Classify samples according to the SVM algorithm
}
\description{
This function performs classification through the Support Vector Machines
(SVM) algorithm. The algorithm applies on the "exemplars" dataset. It
produces a classification result either on the training set or on a
validation set. This function estimates how well the selected "genes" from
mAP-KL method discriminate between two phenotypes.The default SVM settings
are: "linear" kernel and 5-folds cross-validation. Regarding the parameters
for the "linear" kernel ,cost parameter, and for the "radial" kernel, cost
and gamma parameters, are estimated automatically through the tune.svm
function as described in e1071 r-package.
}
\usage{
classification(trExemplObj,classLabels,valExemplObj=NULL,kf=5,kernel="linear")
}

\arguments{
    \item{trExemplObj}{
    The exemplars train eSet object. %%\code{trExemplObj}
    }
    \item{classLabels}{
    The varLabels name in the eSet object where the class labels are stored
    e.g "type". %%\code{classLabels}
    }
    \item{valExemplObj}{
    The exemplars validation eSet object (if not NULL). %%\code{valExemplObj}
    }
    \item{kf}{
    The k-folds value of the cross-validation parameter.The default value is
    5-folds. By setting "Loo" or "LOO" a Leave-One-Out Cross Validation
    is performed %%\code{kf}
    }
    \item{kernel}{
    The type of kernel used for the classification analysis.The default
    kernel is "linear" %%\code{kernel}
    }
}

\value{
    \item{classL }{The labels of the train set}
    \item{valClassL }{The labels of the validation set if not NULL}
    \item{predLbls }{The predicted labels according to the classification
    analysis}
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

exprs(breast$trainData)<-normTrainData$clL2.normdata
exprs(breast$testData)<-normTestData$clL2.normdata

out.clL2 <- mAPKL(trObj=breast$trainData, classLabels="type",
valObj=breast$testData,dataType=7)

clasPred <- classification(trExemplObj=out.clL2@exemplTrain, classLabels="type",
valExemplObj=out.clL2@exemplTest)

}

\keyword{ classif }

