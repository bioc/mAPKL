test_metrics <-
function() {
    
##  Checking whether the confusion matrix is correct through the calculation of
## three meassures (Accuracy,Specificity, and Sensitivity)

val <- c(rep(0,8),rep(1,4))
pred <- c(rep(0,6),1,1,rep(1,3),0)

tn <- 6
tp <- 3
fp <- 2
fn <- 1

accuracy <- (round((tp + tn) / (tp + tn + fp + fn), 2)) * 100
specificity <- round((tn / (tn + fp)), 2)
sensitivity <- round((tp / (tp + fn)), 2)

perfMetrics <- metrics(val,pred)

## 1. Checking Accuracy calculation
checkEqualsNumeric(accuracy,perfMetrics$Accuracy,0)
## 2. Checking Specificity calculation
checkEqualsNumeric(specificity, perfMetrics$Specificity, 0)
## 3. Checking Sensitivity calculation
checkEqualsNumeric(sensitivity, perfMetrics$Sensitivity, 0)

}