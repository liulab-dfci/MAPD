#' @param x A data frame, which includes features as columns.
#' @param y A factor, the first level should be the positive label.
#' @param method Character, specifying the machine learning method.
#' @param fold An integer, specifying the fold for cross validation.
#' @param metric auROC or prROC.
#' @param out One of ROC, AUC, and Pred.
#' @param sampling One of down, up.
#' @param seed An integer.
#' @param ... Other parameter in train.
cvAUC <- function(x, y, bestTune, method = "rf", fold = 10, metric = "auROC", 
                  out = "ROC", sampling = NULL, seed = NULL, ...){
    if(!is.null(seed)) set.seed(seed)
    idx = sample(1:length(y), length(y))
    x = x[idx, , drop=FALSE]; y = y[idx]
    fitControl <- trainControl(method = "none", classProbs = TRUE, sampling = sampling)
    if(metric=="prROC") 
        fitControl <- trainControl(method = "none", classProbs = TRUE, 
                                   summaryFunction = prSummary, sampling = sampling)
    preds = lapply(1:fold, function(i){
        nsam = floor(nrow(x)/fold)
        idx = ((i-1)*nsam+1):(i*nsam)
        if(i == fold) idx = ((i-1)*nsam+1):nrow(x)
        tmpM <- train(x = x[-idx, , drop=FALSE], y = y[-idx], method = method, 
                      trControl = fitControl, tuneGrid = bestTune, ...)
        predict(tmpM, newdata = x[idx, , drop=FALSE], type = "prob")[,1]
    })
    preds = unlist(preds)
    names(preds) = rownames(x)
    roc <- roc(y, preds)
    if(metric=="auROC"){
        if(out=="ROC") return(roc)
        if(out=="AUC") return(roc$auc)
        return(preds)
    }else if(metric=="prROC"){
        labels = as.integer(as.factor(y))
        labels[labels==2] = 0
        pr <- PRROC::pr.curve(weights.class0  = labels, scores.class0 = preds, curve = TRUE) 
        names(pr)[2] = "auc"
        if(out=="ROC") return(pr)
        if(out=="AUC") return(pr$auc)
        if(out=="F"){ 
            F = pr$curve[,1]*pr$curve[,2]*2 / (pr$curve[,1]+pr$curve[,2])
            return(F[which.max(F)])
        }
        return(preds)
    }
}
