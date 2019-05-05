annotate <-
function(exemplars, chip) {

    annotObj <- new("Annot")

    pkg <- chip
    require(pkg, character.only = TRUE)

    info <- select(get(pkg), keys=names(exemplars), columns=c("SYMBOL",
    "ENTREZID","ENSEMBL","MAP"), keytype="PROBEID")
    
    annotObj@results <- info
    annotObj@probe <- info$PROBEID
    annotObj@symbol <- info$SYMBOL
    annotObj@entrezId <- info$ENTREZID
    annotObj@ensemblId <- info$ENSEMBL
    annotObj@map <- info$MAP

    annotObj
}
