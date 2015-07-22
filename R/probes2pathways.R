probes2pathways <-
function(annotObj) {

    
# We first remove the NA entries
genes <- annotObj@entrezId[!is.na(annotObj@entrezId)]

# Then we map the Entrez ID to Reactome pathway identifiers
qExtID2PathID <- mget(genes, reactomeEXTID2PATHID, ifnotfound = NA)
notNA.idx <- unlist(lapply(qExtID2PathID, function(i) !all(is.na(i))))
qExtID2PathID <- qExtID2PathID[notNA.idx]
pathID <- as.character(qExtID2PathID[[1]])

# Finally we map Reactome pathway identifiers to pathway names

unlist(mget(pathID, reactomePATHID2NAME))

    
}