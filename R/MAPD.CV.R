#' Cross-validation.
#'
#' @param model A caret model returned by the train function.
#' @param fold An integer, specifying the fold for cross validation.
#' @param metric A character, One of ROC and PRC.
#' @param out A character, specifying the type of output.
#' Should be one of curve, AUC, F1, and Pred.
#' @return The output depends the `out` parameter. Could be a ROC/PRC object
#' or AUC value or prediction scores collected from the cross-validation.
#' @note This function merges predicted probability from each recursion (each-fold of samples)
#' and evaluate the performance based on prediction scores for all samples.
#' @import caret
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve
#' @export
#'
MAPD.CV <- function(model, fold = 20, metric = "ROC", out = "curve"){
  requireNamespace("caret")
  trainDat = model$trainingData
  x = trainDat[,colnames(trainDat)!=".outcome"]
  y = trainDat$.outcome

  idx = sample(1:length(y), length(y))
  x = x[idx, , drop=FALSE]; y = y[idx]
  fitControl <- trainControl(method = "cv", number = fold, classProbs = TRUE)
  if(toupper(metric)=="ROC") fitControl$summaryFunction = twoClassSummary
  if(toupper(metric)=="PRC") fitControl$summaryFunction = prSummary
  train.metric = ifelse(toupper(metric)=="PRC", "AUC", "ROC")
  preds = lapply(1:fold, function(i){
    nsam = floor(nrow(x)/fold)
    idx = ((i-1)*nsam+1):(i*nsam)
    if(i == fold) idx = ((i-1)*nsam+1):nrow(x)
    tmpM <- train(x = x[-idx, , drop=FALSE], y = y[-idx], metric = train.metric,
                  method = model$method, trControl = fitControl,
                  tuneGrid = model$bestTune, ntree = model$finalModel$param$ntree,
                  nodesize = model$finalModel$param$nodesize)
    predict.train(tmpM, newdata = x[idx, , drop=FALSE], type = "prob")[,1]
  })
  preds = unlist(preds)
  names(preds) = rownames(x)
  if(tolower(out)=="pred") return(preds)

  if(toupper(metric)=="ROC"){
    roc <- pROC::roc(y, preds)
    if(tolower(out)=="curve") return(roc)
    if(toupper(out)=="AUC") return(roc$auc)
  }else if(toupper(metric)=="PRC"){
    labels = as.integer(as.factor(y))
    labels[labels==2] = 0
    pr <- PRROC::pr.curve(weights.class0  = labels, scores.class0 = preds, curve = TRUE)
    names(pr)[2] = "auc"
    if(tolower(out)=="curve") return(pr)
    if(toupper(out)=="AUC") return(pr$auc)
    if(toupper(out)=="F1"){
      F = pr$curve[,1]*pr$curve[,2]*2 / (pr$curve[,1]+pr$curve[,2])
      return(F[which.max(F)])
    }
  }else{
    stop("Unrecognized metric")
  }
}
