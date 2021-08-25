## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----install, eval=FALSE------------------------------------------------------
#  BiocManager::install(c("caret", "doParallel", "pROC", "PRROC"))
#  devtools::install_github("WubingZhang/MAPD")

## ----lib----------------------------------------------------------------------
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(MAPD))

## ----load---------------------------------------------------------------------
# Load feature data
data("featureDat", package = "MAPD")
# Load kinases for training
data("ProteinsForTrain", package = "MAPD")

# You can also load your own data.
# featureDat: a matrix or a data frame, each row represents a gene/protein, each column represents a feature.
# ProteinsForTrain: a factor or a vector, specifying genes/proteins with high or low degradability for training the model. The names of the `ProteinsForTrain` should match the row names of `featureDat`.

## ----select-------------------------------------------------------------------
# Parallel Processing
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method = "LOOCV")
# run the RFE algorithm
results <- rfe(featureDat[names(ProteinsForTrain), ], ProteinsForTrain,
               sizes=seq(2,10,2), rfeControl=control)
# summarize the results
print(results)
# plot the results
plot(results, type=c("g", "o"))
stopCluster(cl)

# In the MAPD manuscript, we used forward feature selection and seleted five important features for predicting degradability, including Ubiquitination_2, Phosphorylation_2, Acetylation_1, Length, Zecha2018_Hela_Halflife

## ----train--------------------------------------------------------------------
# Using default setting: built-in feature data and pre-selected features
cl <- makePSOCKcluster(4); registerDoParallel(cl) # Parallel Processing
MAPD = MAPD.train(class = ProteinsForTrain) # ntree = 20000

# You can also use the optimal feature set selected by rfe (recursive feature elimination) in previous step.
MAPD = MAPD.train(class = ProteinsForTrain, features = results$optVariables, ntree = 100)
stopCluster(cl) # Close the parallel cluster 

## ----evaluate-----------------------------------------------------------------
# You can load pre-trained model using:
# data("MAPD", package = "MAPD")
cl <- makePSOCKcluster(4); registerDoParallel(cl) # Parallel Processing
prc = MAPD.CV(MAPD, metric = "PRC") # Precison-Recall Curve
PRC.plot(prc)
roc = MAPD.CV(MAPD, metric = "ROC") # ROC Curve
ROC.plot(roc)
stopCluster(cl) # Close the parallel cluster 

## ----predict------------------------------------------------------------------
# You can load pre-trained model using:
# data("MAPD", package = "MAPD")
cl <- makePSOCKcluster(4); registerDoParallel(cl) # Parallel Processing

# Predict proteome-wide degradability
preds = predict.train(MAPD, newdata = featureDat, type = "prob")
preds = preds[order(-preds[,1]), ]
head(preds)

# Collect predictions from cross-validation
preds_cv = MAPD.CV(MAPD, out = "pred")
head(sort(preds_cv, decreasing = TRUE))

# Update the predictions for proteins in the training
preds[rownames(preds_cv), 1] = preds_cv
preds[rownames(preds_cv), 2] = 1-preds_cv
stopCluster(cl) # Close the parallel cluster 

