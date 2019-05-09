annotate <-
function(exemplars, chip) {

    annotObj <- new("Annot")

    pkg <- chip
    require(pkg, character.only = TRUE)

    info <- select(get(pkg), keys=names(exemplars), columns=c("SYMBOL",
    "ENTREZID","ENSEMBL","MAP"), keytype="PROBEID")
        
    
    SYMBOL <- mapIds(get(pkg), keys=names(exemplars), column="SYMBOL", keytype="PROBEID", multiVals="first")
    ENTREZID <- mapIds(get(pkg), keys=names(exemplars), column="ENTREZID", keytype="PROBEID", multiVals="first")
    ENSEMBL <- mapIds(get(pkg), keys=names(exemplars), column="ENSEMBL", keytype="PROBEID", multiVals="first")
    MAP <- mapIds(get(pkg), keys=names(exemplars), column="MAP", keytype="PROBEID", multiVals="first")
    
    annotObj@results <- info
    annotObj@probe <- names(exemplars)
    annotObj@symbol <- SYMBOL
    annotObj@entrezId <- ENTREZID
    annotObj@ensemblId <- ENSEMBL
    annotObj@map <- MAP

    annotObj
}
