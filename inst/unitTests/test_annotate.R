test_annotate <-
function() {
    
## 1. if returned probe names are identical to those exported from mAPKL function
## 2. if returned number of columns equals 5 as defined in Annot class
## 3. if returned probes are no NA 
## 4. if returned number of columns equals 5 as defined in Annot class
    
library(mAPKL)
library(mAPKLData)
data(mAPKLData)

breast <- sampling(Data=mAPKLData, valPercent=40, classLabels="type", seed=135)
normTrainData <- preprocess(breast$trainData)
normTestData <- preprocess(breast$testData)
exprs(breast$trainData) <- normTrainData$clL2.normdata
exprs(breast$testData) <- normTestData$clL2.normdata
#out.clL2 <- mAPKL(trObj=breast$trainData, classLabels="type",
#valObj=breast$testData, dataType=7)
out.clL2 <- mAPKL(trObj=breast$trainData, classLabels="type", valObj=breast$testData, dataType=7, statTest="t", 
permutations=1000, features=200, minClusters=2, maxClusters=50, FC="limma", 
bimaxit=50, r=2)
gene.info <- annotate(out.clL2@exemplars, "hgu133plus2.db")

exemplrs <- length(out.clL2@exemplars)
genes <- dim(gene.info@results)[1]

columns <- dim(gene.info@results)[2]


checkEquals(names(out.clL2@exemplars), gene.info@probe)
checkTrue(genes >= exemplrs)
checkTrue(!anyNA(gene.info@probe))
checkEqualsNumeric(5, columns, 0)
}

