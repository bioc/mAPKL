metrics <-
function(classLbls, predLbls) {

    cmp <- mapply(FUN='%in%', predLbls, classLbls)
    true <- classLbls[cmp]

############ TRUE POSITIVE & FALSE NEGATIVE ############

    tp1 <- sum(classLbls)
    tp2 <- sum(true)

    truepos <- tp2
    falseneg <- tp1 -tp2

############ TRUE NEGATIVE & FALSE POSITIVE ############
    trueneg <- length(true) - tp2
    falsepos <- length(classLbls) - (trueneg + truepos + falseneg)

    negative <- trueneg + falsepos
    positive <- truepos + falseneg

    negativestring <- sprintf("Negative samples: %d", negative)
    message(negativestring)
    positivestring <- sprintf("Positive samples: %d", positive)
    message(positivestring)
    tnstring <- sprintf("TN=%d", trueneg)
    message(tnstring)
    fpstring <- sprintf("FP=%d", falsepos)
    message(fpstring)
    tpstring <- sprintf("TP=%d", truepos)
    message(tpstring)
    fnstring <- sprintf("FN=%d", falseneg)
    message(fnstring)

######################## AUC #############################
## next 2 lines: from Wintle et al 2005 supp mat "roc" function
    rnk <- rank(predLbls)
    AUC <- round(((negative * positive) + ((negative * (negative + 1))/2) 
    - sum (rnk[1:negative])) / (negative * positive), 2)
    aucstring <- sprintf("AUC=%.2f", AUC)
    message(aucstring)

    accuracy <- (round((truepos+trueneg) 
    / (truepos+trueneg+falsepos+falseneg), 2)) * 100
    
    accurstring <- sprintf("Accuracy=%2.2f", accuracy)
    message(accurstring)

    arg1 <- ((truepos*trueneg)-(falsepos*falseneg))
    
    arg2 <- sqrt((truepos+falsepos) * (truepos+falseneg) 
    * (trueneg+falsepos) * (trueneg+falseneg))
    
    MCC <- round(arg1/arg2, 2)
    if(is.nan(MCC))    MCC <- 0
    mccstring <- sprintf("MCC=%.2f", MCC)
    message(mccstring)

    specificity <- round((trueneg/(trueneg+falsepos)), 2)
    specifstring <- sprintf("Specificity=%.2f", specificity)
    message(specifstring)

    sensitivity <- round((truepos/(truepos+falseneg)), 2)
    sensitstring <- sprintf("Sensitivity=%.2f", sensitivity)
    message(sensitstring)


    list(
        AUC = AUC,
        Accuracy = accuracy,
        MCC = MCC,
        Specificity = specificity,
        Sensitivity = sensitivity
        )
}