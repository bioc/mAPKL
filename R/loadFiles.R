loadFiles <-
function(filesPath, trainFile, labelsFile, validationFile=NULL, 
validationLabels=NULL) {

    dataObj <- new("DataLD")

    setwd(filesPath)

    expfile1 <- paste(filesPath, sprintf("%s", trainFile), sep="")
    trainset <- read.delim(expfile1, TRUE, row.names=1)
    intensTrain <- data.matrix(trainset)

    expfile2 <- paste(filesPath, sprintf("%s", labelsFile), sep="")
    classL <- read.delim(expfile2, row.names = 1, header=TRUE)

    phenoData <- new("AnnotatedDataFrame", data=classL)

    dataObj@trainObj <- ExpressionSet(assayData=intensTrain, 
    phenoData=phenoData)

    Treatment <- sum(classL)

    Control <- length(classL[,1]) - Treatment
    samples <- sprintf("Number of Control samples=%d and Treatment samples=%d",
    Control, Treatment)

    message(samples)

    idx <- order(classL, decreasing="FALSE")
    ordCls <- classL[,1][idx]

    if(identical(classL[,1],ordCls)) {
        message("Samples are ordered according to '0' and '1' labels")
        startidx <- Control + 1
        endidx <- Control + Treatment
        dim_disease <- sprintf("The Treatment samples range between columns:
        %d to %d", startidx, endidx)
        
        message(paste(strwrap(dim_disease, exdent=2), collapse="\n"))
    }

    else message("Samples are not ordered according to labels '0' and '1'")


    if(!is.null(validationFile)) {

        expfile <- paste(filesPath, sprintf("%s", validationFile), sep="")
        testset <- read.delim(expfile, TRUE, row.names=1)
        intensTest <- data.matrix(testset)

        expfile3 <- paste(filesPath,sprintf("%s", validationLabels), sep="")
        valClassL <- read.delim(expfile3, row.names = 1, header=TRUE)

        phenoData <- new("AnnotatedDataFrame", data=valClassL)

        dataObj@valObj <- ExpressionSet(assayData=intensTest, 
        phenoData=phenoData)
    }

    dataObj

}
