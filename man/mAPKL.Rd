\name{mAPKL}
\alias{mAPKL}
\title{
    The mAP-KL algorithm
}
\description{
We first employ the mt.maxT function from the "multtest" package to rank the
genes of the training set and then we reserve the top N genes e.g.(N = 200)
for further exploitation. Prior to clustering analysis with Affinity
Propagation we apply the index of Krzanowski and Lai as included in the
"ClusterSim" package to determine the number of clusters solely on the
disease samples of the training test set. The final step involves the cluster
analysis with the AP clustering method as in the "apcluster" package, which
detects n (n = k, the Krzanowski and Lai index) clusters among the top N
genes and provides us with a list of the most representative genes of each
cluster, the so called exemplars.
}
\usage{
mAPKL(trObj,classLabels,valObj=NULL,dataType=6,statTest="t",permutations=1000,
features=200,minClusters=2,maxClusters=50,FC="limma",bimaxit=50,r=2)
}
\arguments{
    \item{trObj}{
    The train eSet object. %%\code{trObj}
    }
    \item{classLabels}{
    The varLabels name in the eSet object where the class labels are stored
    e.g "type".%%\code{classLabels}
    }
    \item{valObj}{
    The validation eSet object (if not NULL). %%\code{valObj}
    }
    \item{dataType}{
    The type of the data e.g 6-ratio data without normalization and 7-interval
    or mixed (ratio & interval) data without normalization as described in
    "clusterSim" package. %%\code{dataType}
    }
    \item{statTest}{
    The statistical test applied to the geneIntensities. The available tests
    described in mt.maxT documentation in "multtest" package. %%\code{statTest}
    }
    \item{permutations}{
    The number of permutations. %%\code{permutations}
    }
    \item{features}{
    The top N genes to be kept. %%\code{features}
    }
    \item{minClusters}{
    The minimum number of clusters that can be identified. %%\code{minClusters}
    }
    \item{maxClusters}{
    The maximum number of clusters that can be identified. %%\code{maxClusters}
    }
    \item{FC}{
    The Fold Change of the exemplars according to "Limma" (default).
    Alternatively the "SAM" approach may be computed.  %%\code{FC}
    }
    \item{bimaxit}{
    The maximum number of bisection steps performed by the AP algorithm. The
    (default) value is "50".  %%\code{bimaxit}
     }
     \item{r}{
    The argument r is used to transform the resulting distances by computing the
    r-th power. To obtain negative squared distances as in Frey's and
    Dueck's(use r=2 as default). %%\code{r}
    }
}

\value{
    \item{rankedIntens }{The top N ranked genes with their intensity values}
    \item{exemplTrain }{The intensity values of the exemplars in the
    training set}
    \item{exemplTest }{The intensity values of the exemplars in the validation
    set if not NULL}
    \item{statistic }{A list with the overall results of the "mt.maxT" analysis}
    \item{adjp }{The adjusted p-values according to the statistical analysis}
    \item{pVal }{The raw p-values according to the statistical analysis}
    \item{fc }{The Fold Change of the exemplars}
    \item{exemplars }{The selected "significant" probe ids/genes}
    \item{clusters }{The probe ids/genes per cluster}
}
\references{
    A. Sakellariou, D. Sanoudou, and G. Spyrou, "Combining multiple hypothesis
    testing and affinity propagation clustering leads to accurate, robust and
    sample size independent classification on gene expression data,"
    BMC Bioinformatics, vol. 13, p. 270, 2012. \cr
}
\author{
    Argiris Sakellariou
}
\examples{
## Using separate train-test samples
## Load the necessary files based on Breast cancer data as included in the
## package mAPKLData

library(mAPKLData)
data(mAPKLData)
breast <- sampling(Data=mAPKLData, valPercent=40, classLabels="type", seed=135)
normTrainData <- preprocess(breast$trainData)
normTestData <- preprocess(breast$testData)

exprs(breast$trainData) <- normTrainData$clL2.normdata
exprs(breast$testData) <- normTestData$clL2.normdata

out.clL2 <- mAPKL(trObj=breast$trainData, classLabels="type",
valObj=breast$testData, dataType=7)

}
\keyword{ htest }

