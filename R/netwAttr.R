netwAttr <-
function(mAPKLObj, net="clr") {

    netObj <- new("NetAttr")
    

    mi <- knnmi.all(mAPKLObj@rankedIntens)
    
    if(net=="aracne.a")
        grn.net <- aracne.a(mi,0.05)
      
    else if(net=="aracne.m")
        grn.net <- aracne.m(mi,0.15)
      
    else
        grn.net <- clr(mi)
      
      
    graph.net <- graph.adjacency(grn.net, weighted=TRUE)
    edgelist <- get.edgelist(graph.net)
    edgelist <- cbind(edgelist, rep(0, length(edgelist[,1])))
    edgelist[,3] <- E(graph.net)$weight   ## This object is an output
    colnames(edgelist) <- c("#Node1", "Node2", "Weight")
    netObj@edgelist <- edgelist

    DegreeLocal <- degree(graph.net)
    DegreeGlobal <- mean(DegreeLocal)
    ## weighted degree
    WDegreeLocal <- graph.strength(graph.net, weights=edgelist[,3])
    WDegreeGlobal <- mean(WDegreeLocal)

    netObj@degree <- list(
            degreeL = DegreeLocal,
            degreeG = DegreeGlobal,
            WdegreeL = round(WDegreeLocal, 2),
            WdegreeG = WDegreeGlobal
            )

    WClosenessLocal <- closeness(graph.net, weights=edgelist[,3])
    WClosenessGlobal <- mean(WClosenessLocal)

    netObj@closeness <- list(
            WclosenessL = round(WClosenessLocal, 2),
            WclosenessG = WClosenessGlobal
            )

    WBetweennessLocal <- betweenness(graph.net, weights=edgelist[,3])
    WBetweennessGlobal <- mean(WBetweennessLocal)

    netObj@betweenness <- list(
            WbetweennessL = round(WBetweennessLocal, 2),
            WbetweennessG = WBetweennessGlobal
            )

    WClusteringCoefLocal <- transitivity(graph.net, type="local", 
    weights=edgelist[,3])
    
    WClusteringCoefGlobal <- transitivity(graph.net, type="global", 
    weights=edgelist[,3])

    netObj@transitivity <- list(
            WtransitivityL = round(WClusteringCoefLocal, 2),
            WtransitivityG = WClusteringCoefGlobal
            )
    netObj
}