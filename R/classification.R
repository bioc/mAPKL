## Here starts my function for dealing with
## LIBSVM library for Support Vector Vector Machines Classification
## kf-> The number of folds (by default is 5-folds)
## kernel->The type of kernel in SVM. By default the "linear" kernel otherwise
## the "radial"

classification <-
function(trExemplObj, classLabels, valExemplObj=NULL, kf=5, kernel="linear") {

    clfcObj <- new("Classify")

    train.mtx <- aperm(exprs(trExemplObj))


    cls <- table(pData(trExemplObj)[classLabels])
    tn <- cls[[1]]
    tp <- cls[[2]]


    if(kf == "Loo" | kf == "LOO") {

        k_fold <- tp
        cross_validation <- sprintf("The training set has %d Negative and %d
        Positive samples. Using Leave-One-Out C-V (%d folds)", tn, tp, k_fold)

        message(paste(strwrap(cross_validation, exdent=2), collapse="\n"))
    }
    else {

        k_fold <- kf
        cross_validation <- sprintf("The training set has %d Negative and %d
        Positive samples. Using k-fold=%d C-V", tn, tp, k_fold)

        message(paste(strwrap(cross_validation, exdent=2), collapse="\n"))
    }

    lbls <- pData(trExemplObj)[classLabels]


    out1 <- "############ THE BEST PARAMETERS TUNING STAGE ##################"
    message(out1)
    obj <- tune.svm(train.mtx, lbls, gamma = 2^(3:-3), cost = 2^(-2:6),
    tunecontrol=tune.control(sampling="cross", cross=k_fold))

    best <- obj$best.parameters
    best_gamma <- best$gamma
    best_cost <- best$cost

    out2 <- "############# THE TRAINING STAGE ########################"
    message(out2)

    if(kernel == "radial") {

        model <- svm(train.mtx, lbls,scale=FALSE, type="C-classification",
        kernel="radial", gamma=best_gamma, cost=best_cost, cross=k_fold)
    }
    else
        model <- svm(train.mtx, lbls, scale=FALSE, type="C-classification",
        kernel="linear", gamma=best_gamma, cost=best_cost, cross=k_fold)

    print(model)

    out3 <- "############# THE PREDICTION STAGE ######################"
    message(out3)

    colnames(lbls) <- "Train Labels"
    clfcObj@classL <- as.matrix(lbls)

    if(is.null(valExemplObj)) {
        predictions <- as.character(predict(model, train.mtx))
        predictions <- array(as.integer(predictions), 
        dim=c(length(predictions), 1))
        
        colnames(predictions) <- "Prediction Labels"

        print(outcome <- cbind(lbls, predictions))

        rownames(predictions) <- sampleNames(trExemplObj)
        clfcObj@predLbls <- predictions
    
## Evaluate classification
        perfMetrics <- metrics(clfcObj@classL, clfcObj@predLbls)

        clfcObj@AUC <- perfMetrics$AUC
        clfcObj@Accuracy <- perfMetrics$Accuracy
        clfcObj@MCC <- perfMetrics$MCC
        clfcObj@Specificity <- perfMetrics$Specificity
        clfcObj@Sensitivity <- perfMetrics$Sensitivity

    }

    else {
        val.mtx <- aperm(exprs(valExemplObj))
        tstsamples <- sampleNames(valExemplObj)
        val.labels <- pData(valExemplObj)[classLabels]
        predictions <- as.character(predict(model, val.mtx))
        predictions <- array(as.integer(predictions), 
        dim=c(length(predictions), 1))

        colnames(val.labels) <- "Test Labels"
        colnames(predictions) <- "Prediction Labels"
        rownames(predictions) <- tstsamples

        print(cbind(val.labels, predictions))

        clfcObj@valClassL <- as.matrix(val.labels)
        clfcObj@predLbls <- predictions
        
## Evaluate classification
        perfMetrics <- metrics(clfcObj@valClassL, clfcObj@predLbls)
        
        clfcObj@AUC <- perfMetrics$AUC
        clfcObj@Accuracy <- perfMetrics$Accuracy
        clfcObj@MCC <- perfMetrics$MCC
        clfcObj@Specificity <- perfMetrics$Specificity
        clfcObj@Sensitivity <- perfMetrics$Sensitivity

    }
    clfcObj
}
