sampling <-
function(Data, valPercent, classLabels, seed=1) {

    if(valPercent < 10)    stop("test set percentage < 10% is unsuitable...")

    else {
        gexprs <- exprs(Data)
        prcnt <- valPercent / 100
    }
    msg <- sprintf("Assign %d%% as train data and %d%% as test data", 
    100 - valPercent, valPercent)
    message(paste(strwrap(msg, exdent=2), collapse="\n"))
## Assign valPercent as test data and the rest as training data

    set.seed(seed)
    nTest <- ceiling(ncol(gexprs) * prcnt )
    TestDataColInd <- sample(ncol(gexprs), nTest)

    #dataTest <- gexprs[, TestDataColInd]
    idx1 <- order(pData(Data)[TestDataColInd,][classLabels], 
    decreasing ='FALSE')
    
    dataTest <- gexprs[, TestDataColInd] [,idx1]
    phenoTest <- phenoData(Data[,TestDataColInd])[idx1]

    idx2 <- order(pData(Data)[-TestDataColInd,][classLabels], 
    decreasing ='FALSE')
    
    dataTrain <- gexprs[, -TestDataColInd] [,idx2]
    phenoTrain <- phenoData(Data[,-TestDataColInd])[idx2]

    trainData <- ExpressionSet(assayData=dataTrain, phenoData=phenoTrain,
    experimentData=experimentData(Data), annotation=annotation(Data))

    testData <- ExpressionSet(assayData=dataTest, phenoData=phenoTest,
    experimentData=experimentData(Data), annotation=annotation(Data))

    list(
        trainData = trainData,
        testData = testData
    )
}


