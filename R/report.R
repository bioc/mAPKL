report <-
function(mAPKLObj, ClassifyObj, AnnotObj=NULL, netObj=NULL, file) {

## Retrieve the system's current date-time.
## It is used as a timestamp of the report
    timestmp <- format(Sys.time(), "%a %b %d %H:%M:%S %Y")

    cat("<style>
    H1 {
        font-family: Arial, Helvetica, sans-serif;
        font-size: 20pt;
        font-style: normal;
        font-weight: normal;
        color: #FFFFFF;
        background: #004080;
        text-align: center;
        margin: 10pt

    }
    H2 {
        font-family: Arial, Helvetica, sans-serif;
        font-size: 16pt;
        font-style: italic;
        font-weight: normal;
        color: #FFFFFF;
        background: #000000;
        text-align: center;
        margin: 10pt 5%
    }
    H4 {
        font-family: Arial, Helvetica, sans-serif;
        font-size: 08pt;
        font-style: italic;
        font-weight: bold;
        text-align: left;
        margin: 10pt 5%
    }
    body {
        background: #FFFFFF;
        color: #000000;
        font-family: Verdana, Arial, Helvetica, sans-serif;
        font-size: 09pt;
        font-weight: normal
    }
    table, td, th {
        border: 1px solid blue;
        padding: 3px 7px 2px 7px;
        font-size: 08pt;
    }
    th {
        background-color: blue;
        color: white;
    } </style>",file = file, append = TRUE)
## Start building the mAP-KL analysis report
    cat(sprintf("<H1>mAP-<i>KL</i> Analysis Report</H1><H4>%s</H4>", timestmp),
    file=file, append=TRUE)
    cat("\n <body ", file = file, append = TRUE)
    cat("<hr>",file=file, append=TRUE)
    cat("<H2>Data Samples</H2>", file=file, append=TRUE)

## In cases where only a training set is available
    if(!length(ClassifyObj@valClassL)) {
        samples.tr <- rownames(ClassifyObj@classL)
        lng.tr <- length(ClassifyObj@classL)
        cat("<table align=center border=1> <tr>
        <th align=center><b>Training Set</th>
        <th align=center><b>Class Labels</th>", file=file, append=TRUE)

    for(i in 1:lng.tr) {
        cat(sprintf("<tr> <td>%s</td> <td align=center>%s</td> </tr>",
        samples.tr[i], ClassifyObj@classL[i]), file=file, append=TRUE)
        }
    cat("</table><br>", file=file, append=TRUE)
    }

    else {
        samples.tr <- rownames(ClassifyObj@classL)
        samples.val <- rownames(ClassifyObj@valClassL)
        lng.tr <- length(ClassifyObj@classL)
        lng.val <- length(ClassifyObj@valClassL)

    if(lng.tr > lng.val) {

        lng.s <- lng.val
        rmn <- lng.tr - lng.val
        ind1 <- lng.s +1
        ind2 <- lng.s + rmn

        cat("<table align=center border=1> <tr>
        <th align=center><b>Training Set</th>
        <th align=center><b>Class Labels</th>
        <th align=center><b>Validation Set</th>
        <th align=center><b>Class Labels</th>", file=file, append=TRUE)

    for(j in 1:lng.s) {

        cat(sprintf("<tr> <td>%s</td> <td align=center>%s</td>
        <td>%s</td> <td align=center>%s</td> </tr>",
        samples.tr[j], ClassifyObj@classL[j], samples.val[j],
        ClassifyObj@valClassL[j]), file=file, append=TRUE)

        }

    for(k in ind1:ind2) {

        cat(sprintf("<tr> <td>%s</td> <td align=center>%s</td> <td></td>
        <td align=center></td> </tr>",
        samples.tr[k], ClassifyObj@classL[k]), file=file, append=TRUE)
        }

    }
    else {

        lng.s <- lng.tr
        rmn <- lng.val - lng.tr
        ind1 <- lng.s +1
        ind2 <- lng.s + rmn

        cat("<table align=center border=1> <tr>
        <th align=center><b>Training Set</th>
        <th align=center><b>Class Labels</th>
        <th align=center><b>Validation Set</th>
        <th align=center><b>Class Labels</th>", file=file, append=TRUE)

    for(j in 1:lng.s) {

        cat(sprintf("<tr> <td>%s</td> <td align=center>%s</td> <td>%s</td>
        <td align=center>%s</td> </tr>",
        samples.tr[j], ClassifyObj@classL[j], samples.val[j],
        ClassifyObj@valClassL[j]), file=file, append=TRUE)
    }

    for(k in ind1:ind2) {

        cat(sprintf("<tr> <td></td> <td align=center></td> <td>%s</td>
        <td align=center>%s</td> </tr>",
        samples.val[k], ClassifyObj@valClassL[k]), file=file, append=TRUE)
        }

    }

    cat("</table><br>",file=file, append=TRUE)

    }
    cat("<hr>",file=file, append=TRUE)

    cat("<H2>Exemplars</H2>", file=file, append=TRUE)

    lng.ex <- length(mAPKLObj@exemplars)
    indx <- mAPKLObj@exemplars
    if(is.null(netObj)) {
        cat("<table border=1 align=center width=40%> <tr>
        <th align=center><b>Exemplars </th>
        <th align=center><b>Adj.p-value</th>
        <th align=center><b>p-value</th>
        <th align=center><b>FC</th> </tr>", file=file, append=TRUE)

    for(m in 1:lng.ex) {
        cat(sprintf("<tr> <td>%s</td> <td align=center>%s</td>
        <td align=center>%s</td> <td align=center>%s</td> </tr>",
        names(mAPKLObj@exemplars[m]), mAPKLObj@adjp[m], mAPKLObj@pVal[m],
        mAPKLObj@fc[m]), file=file, append=TRUE)
        }
    }

    else {
        cat("<table border=1 align=center width=40%> <tr>
        <th align=center><b>Exemplars </th>
        <th align=center><b>Adj.p-value</th>
        <th align=center><b>p-value</th>
        <th align=center><b>FC</th>
        <th align=center><b>wL.degree</th>
        <th align=center><b>wL.closeness</th>
        <th align=center><b>wL.betweenness</th>
        <th align=center><b>wL.transitivity</th> </tr>", file=file, append=TRUE)

    for(m in 1:lng.ex) {
        cat(sprintf("<tr> <td>%s</td> <td align=center>%s</td>
        <td align=center>%s</td> <td align=center>%s</td>
        <td align=center>%s</td> <td align=center>%s</td>
        <td align=center>%s</td> <td align=center>%s</td> </tr>",
        names(mAPKLObj@exemplars[m]), mAPKLObj@adjp[m], mAPKLObj@pVal[m],
        mAPKLObj@fc[m], netObj@degree$WdegreeL[indx[m]],
        netObj@closeness$WclosenessL[indx[m]],
        netObj@betweenness$WbetweennessL[indx[m]],
        netObj@transitivity$WtransitivityL[indx[m]]), file=file, append=TRUE)
        }
    }

    cat("</table><br>", file=file, append=TRUE)
    cat("<hr>", file=file, append=TRUE)

    cat("<H2>Classification Performance</H2>", file=file, append=TRUE)

    if(is.null(ClassifyObj@valClassL))
    cat("<H4>(Cross-Validation)</H4>", file=file, append=TRUE)

    else
    cat("<H4>(Hold-out Validation)</H4>", file=file, append=TRUE)

    cat("<table border=1 align=center width=50%> <tr>
    <th align=center><b>AUC </th>
    <th align=center><b>Accuracy</th>
    <th align=center><b>MCC</th>
    <th align=center><b>Specificity</th>
    <th align=center><b>Sensitivity</th></tr>", file=file, append=TRUE)

    cat(sprintf("<tr> <td align=center>%.2f</td>
    <td align=center>%.2f</td>
    <td align=center>%.2f</td>
    <td align=center>%.2f</td>
    <td align=center>%.2f</td></tr></table>",
    ClassifyObj@AUC, ClassifyObj@Accuracy, ClassifyObj@MCC,
    ClassifyObj@Specificity, ClassifyObj@Sensitivity), file=file, append=TRUE)

    cat("<br>", file = file, append = TRUE)


## Only if an annotation object is available
    if(!is.null(AnnotObj)) {
    cat("<hr>", file=file, append=TRUE)

    cat("<H2>Genome Annotation</H2>", file=file, append=TRUE)
    lng <- length(AnnotObj@probe)

    cat("<table border=1 align=center width=50%> <tr>
    <th align=center><b>PROBE ID </th>
    <th align=center><b>SYMBOL</th>
    <th align=center><b>ENTREZ ID</th>
    <th align=center><b>ENSEMBL ID</th>
    <th align=right><b>MAP</th></tr>", file=file, append=TRUE)

    for(i in 1:lng) {
        cat(sprintf("<tr> <td align=center>%s</td>
        <td align=center>%s</td>
        <td align=center>%s</td>
        <td align=center>%s</td>
        <td align=right>%s</td> </tr>",
        AnnotObj@probe[i], AnnotObj@symbol[i], AnnotObj@entrezId[i],
        AnnotObj@ensemblId[i], AnnotObj@map[i]), file=file, append=TRUE)
    }

    cat("</table>", file= file, append=TRUE)
    }

    cat("<br><hr></body>", file= file, append=TRUE)
    cat(paste("Report written: ", file= file, sep = ""))
}