## S4 class definition for mAPKL package
setClass("mAPKLRes",
representation
    (
        rankedIntens = "matrix",
        exemplTrain = "eSet",
        exemplTest = "eSet",
        statistic = "numeric",
        adjp = "numeric",
        pVal = "numeric",
        fc = "numeric",
        exemplars = "numeric",
        clusters = "list"
    )
)

setClass("DataLD",
representation
    (
        trainObj = "eSet",
        valObj = "eSet"
    )
)

setClass("Classify",
representation
    (
        classL = "matrix",
        valClassL = "matrix",
        predLbls = "matrix",
        AUC = "numeric",
        Accuracy = "numeric",
        MCC = "numeric",
        Specificity = "numeric",
        Sensitivity = "numeric"
    )
)

setClass("Annot",
representation
    (
        results = "list",
        probe = "character",
        symbol = "character",
        entrezId = "character",
        ensemblId = "character",
        map = "character"
    )
)

setClass("NetAttr",
representation
    (
        edgelist = "matrix",
        degree = "list",
        closeness = "list",
        betweenness = "list",
        transitivity = "list"
    )
)
