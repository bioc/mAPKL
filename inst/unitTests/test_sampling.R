test_sampling <-
function() {
    
## 1 & 2. if returned objects are of eSet class
## 3 & 4. if number of samples per eSet class object equals the defined split percentage 

    
library(mAPKLData)
data(mAPKLData)

breast <- sampling(Data=mAPKLData, valPercent=40, classLabels="type", seed=135)

allSamples <- dim(exprs(mAPKLData))[2]

tstSamples <- allSamples * 40 / 100
trSamples <- allSamples - tstSamples

checkTrue(class(breast$trainData) == 'ExpressionSet')
checkTrue(class(breast$testData) == 'ExpressionSet')
checkEqualsNumeric(trSamples, dim(exprs(breast$trainData))[2], 0)
checkEqualsNumeric(tstSamples, dim(exprs(breast$testData))[2], 0)
}