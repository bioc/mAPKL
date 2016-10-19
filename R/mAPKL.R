mAPKL <-
function(trObj, classLabels, valObj=NULL, dataType=6, statTest="t", 
permutations=1000, features=200, minClusters=2, maxClusters=50, FC="limma", 
bimaxit=50, r=2) {

    topGenes <- as.integer(features)
    st.test <- as.character(statTest)
    perm <- as.integer(permutations)
    mAPKLObj <- new("mAPKLRes")

## Check whether the expression data are ordered according to class of '0' and
## '1' labels and if not a new ordered eset object is created
    initCls <- pData(trObj)[,classLabels]
    idx2 <- order(pData(trObj)[classLabels], decreasing="FALSE")
    ordCls <- pData(trObj)[,classLabels] [idx2]
    ordTrExprs <- exprs(trObj) [,idx2]
    ordPhenoTr <- phenoData(trObj)[idx2]
    
    if(identical(initCls, ordCls)) {
        Data <- trObj
    }
    else {
        
        Data <- ExpressionSet(assayData=ordTrExprs, phenoData=ordPhenoTr,
        experimentData=experimentData(trObj), annotation=annotation(trObj))

        message("eSet object was modified")
    }

## Runing the MTP function from the "multtest" package to rank the genes
## according to the adjacent p-value

    resT <- MTP(X=exprs(Data), Y=pData(Data)[classLabels], nulldist='perm',
    method='sd.maxT', seed=99)

    index <- order(abs(resT@statistic), decreasing ='TRUE')

    mAPKLObj@statistic <- resT@statistic[index]
    mAPKLObj@pVal <- resT@rawp[index]
    mAPKLObj@adjp <- resT@adjp[index]

## Create the ordered gene intensities data frame
    ordIntensities <- exprs(Data)[index,]
    genesM <- featureNames(Data)[index]
    ordTrainsetM <- data.frame(genesM, ordIntensities)

## Deciding on the number of clusters

    ordIntensities_f <- ordIntensities[1:features,]
    genes_f <- array(genesM, dim=c(features,1))

## We asked for num of clusters "ONLY IN THE DISEASE SAMPLES" among the top 200
## ranked genes during our study
## path : 6-ratio data without normalization    or
##        7-interval or mixed (ratio & interval) data without normalization
## the minimum Cluster Number is: 2  and
## the maximum Cluster Number is: 50
## KL: is the  Krzanowski & Lai index
## the clustering method is m5:k-medoids (PAM)

    cls <- table(pData(Data)[classLabels])
    start <- as.integer(cls[[1]] + 1)
    end <- as.integer(cls[[1]] + cls[[2]])
    path <- as.integer(dataType)
    min.clu <- as.integer(minClusters)
    max.clu <- as.integer(maxClusters)

    message("Please wait! The (KL) cluster indexing may take several minutes...")
    cluster_analysis <- cluster.Sim(ordIntensities_f[,start:end], path, min.clu,
    max.clu, "KL", methods="m5")

    KL <- as.integer(cluster_analysis$classes)
    message(sprintf("Asking for %d number of clusters", KL))

## Select the exemplars "ONLY IN THE DISEASE SAMPLES" from the 200 ranked list 
## according to the KL index

    s <- negDistMat(ordIntensities_f[,start:end], r=r)
    
    apres <- apclusterK(s, K=KL, prc=0, bimaxit=bimaxit, 
    exact=FALSE, verbose=FALSE)
    
    exemplars <- names(apres@exemplars)
    exemplIndex <- as.numeric(apres@exemplars)

    mAPKLObj@exemplars <- apres@exemplars
    mAPKLObj@clusters <- apres@clusters


    if(FC == "limma") {
        message("fc according to limma")
        
        fc <- log2(2^( rowMeans(ordIntensities_f[exemplIndex,start:end])) 
        / (2^(rowMeans(ordIntensities_f[exemplIndex,1:start-1]))))
        
        fc <- round(fc, 3)
        mAPKLObj@fc <- fc
    }

    else {
        message("fc according to SAM")
        
        fc <- 2^( rowMeans(ordIntensities_f[exemplIndex, 1:start-1]) 
        - rowMeans(ordIntensities_f[exemplIndex, start:end]))
        fc <- round(fc, 3)
        
        mAPKLObj@fc <- fc
    }
    apData <- ExpressionSet(assayData=ordIntensities_f[exemplIndex,], phenoData=ordPhenoTr,
        experimentData=experimentData(trObj), annotation=annotation(trObj))
    mAPKLObj@rankedIntens <- ordIntensities_f[,start:end]
    # exprs(Data) <- ordIntensities_f[exemplIndex,]
    mAPKLObj@exemplTrain <- apData


    if(!is.null(valObj)) {
    
        idx3 <- order(pData(valObj)[classLabels], decreasing="FALSE")
        ordPhenoTst <- phenoData(valObj)[idx3]
        ordIntensTst <- exprs(valObj)[index,]
        valData <- ExpressionSet(assayData=ordIntensTst[exemplIndex,], phenoData=ordPhenoTst,
        experimentData=experimentData(valObj), annotation=annotation(valObj))
        # exprs(valObj) <- ordIntensTst[exemplIndex,]
        mAPKLObj@exemplTest <- valData

    }

    mAPKLObj

}
