netwAttr <-
function(mAPKLObj) {

    netObj <- new("NetAttr")

    mi <- knnmi.all(mAPKLObj@rankedIntens)
    grn.clr <- clr(mi)

    graph.clr <- graph.adjacency(grn.clr, weighted=TRUE)
    edgelist <- get.edgelist(graph.clr)
    edgelist <- cbind(edgelist, rep(0, length(edgelist[,1])))
    edgelist[,3] <- E(graph.clr)$weight   ## This object is an output
    colnames(edgelist) <- c("#Node1", "Node2", "Weight")
    netObj@edgelist <- edgelist

    DegreeLocal <- degree(graph.clr)
    DegreeGlobal <- mean(DegreeLocal)
    ## weighted degree
    WDegreeLocal <- graph.strength(graph.clr, weights=edgelist[,3])
    WDegreeGlobal <- mean(WDegreeLocal)

    netObj@degree <- list(
            degreeL = DegreeLocal,
            degreeG = DegreeGlobal,
            WdegreeL = round(WDegreeLocal, 2),
            WdegreeG = WDegreeGlobal
            )

    WClosenessLocal <- closeness(graph.clr, weights=edgelist[,3])
    WClosenessGlobal <- mean(WClosenessLocal)

    netObj@closeness <- list(
            WclosenessL = round(WClosenessLocal, 2),
            WclosenessG = WClosenessGlobal
            )

    WBetweennessLocal <- betweenness(graph.clr, weights=edgelist[,3])
    WBetweennessGlobal <- mean(WBetweennessLocal)

    netObj@betweenness <- list(
            WbetweennessL = round(WBetweennessLocal, 2),
            WbetweennessG = WBetweennessGlobal
            )

    WClusteringCoefLocal <- transitivity(graph.clr, type="local", 
    weights=edgelist[,3])
    
    WClusteringCoefGlobal <- transitivity(graph.clr, type="global", 
    weights=edgelist[,3])

    netObj@transitivity <- list(
            WtransitivityL = round(WClusteringCoefLocal, 2),
            WtransitivityG = WClusteringCoefGlobal
            )
    netObj
}