#' Train a random forest model (MAPD) for predicting protein degradability.
#'
#' @param class A vector or factor, indicating the class label of proteins for training.
#' The vector or factor should be named with official gene names. The first-level items will be
#' considered as negative (e.g. 0), and the others will be positive (e.g. 1).
#' @param featureDat A matrix or data frame, specifying the user-defined protein feature data.
#' By default, this function will use the internal feature data for training.
#' @param features Protein intrinsic features used for predicting degradability.
#' By default, the five features, including Ubiquitination_2, Phosphorylation_2,
#' Acetylation_1, Zecha2018_Hela_Halflife, Length will be used for training MAPD.
#' The full list of features are available at http://mapd.cistrome.org/.
#' @param ntree An integer, specifying the number of trees in the model.
#' @param summaryFunction A summary function for tuning parameter, prSummary is used by default.
#' @param metric A character, specifying the metric used for tuning parameter.
#'
#' @return The trained model is returned. It is a list.
#' @importFrom caret trainControl prSummary train
#' @import utils
#' @export
#'
MAPD.train <- function(class, featureDat = NULL, features = NULL, ntree = 20000,
                       summaryFunction = caret::prSummary, metric = "AUC"){
  requireNamespace("utils")
  requireNamespace("caret")
  if(is.null(featureDat)){
    utils::data("featureDat", package = "MAPD", envir = environment())
  }
  # Check features
  if(is.null(features)){
    features = c("Ubiquitination_2", "Phosphorylation_2", "Acetylation_1",
                 "Zecha2018_Hela_Halflife", "Length")
  }
  eFeatures = setdiff(features, colnames(featureDat))
  if(length(eFeatures)>0){
    warning("Unavailable features in the training data: ", paste(eFeatures, collapse = ", "))
  }
  features = intersect(features, colnames(featureDat))
  if(length(features)>0){
    message(Sys.time(), " Features used for training: ", paste(features, collapse = ", "))
  }
  featureDat = featureDat[, features, drop = FALSE]

  # Check gene names and class labels.
  class = sort(class)
  newclass = rep("high", length(class))
  newclass[class==class[1]] = "low"
  names(newclass) = names(class)
  eProteins = setdiff(names(newclass), rownames(featureDat))
  if(length(eProteins)>0){
    warning("Unavailable genes in the `class`: ", paste(eProteins, collapse = ", "))
  }
  proteins = intersect(names(newclass), rownames(featureDat))
  featureDat = featureDat[proteins, ]
  idx = colSums(featureDat==0|is.na(featureDat))>nrow(featureDat)*0.9
  if(sum(idx)>0) featureDat = featureDat[, !idx]
  message(Sys.time(), " ", nrow(featureDat), " proteins and ",
          ncol(featureDat), " features are included in the training data")

  # Train the model
  featureDat = as.data.frame(featureDat)
  featureDat$Degradability = newclass[rownames(featureDat)]
  message(Sys.time(), " Start training ...")
  cvmethod = ifelse(nrow(featureDat)>200, "cv", "LOOCV")
  fitControl <- caret::trainControl(method = cvmethod, summaryFunction = caret::prSummary, classProbs = TRUE)
  final <- caret::train(Degradability~., data = featureDat, method = "rf",
                        ntree = ntree, nodesize = 5, trControl = fitControl,
                        importance = TRUE, tuneLength = 5, metric = metric)
  message(Sys.time(), " Return the final model ...")
  return(final)
}

