library(tidyverse)

# Categories of all the features
Feature_cate <- read_tsv("./Data/Features_Category.txt")

# All Features
Feature_info <- readRDS("./Data/Integrated_Features_level1_2021-07-18.rds")
Feature_info <- Feature_info %>%
  dplyr::select(c(colnames(Feature_info)[1:4], Feature_cate$Feature))
Feature_info[,unlist(lapply(Feature_info, is.numeric))] <- 
  round(Feature_info[,unlist(lapply(Feature_info, is.numeric))], 3)
rownames(Feature_info) <- NULL
All_features <- Feature_cate$Feature

# Prediction and Ligandability
Prediction_Ligandability <- readRDS("./Data/Predictions_Ligandability.rds") %>%
  rownames_to_column(var = "Gene")
Prediction_Ligandability[,unlist(lapply(Prediction_Ligandability, is.numeric))] <- 
  round(Prediction_Ligandability[,unlist(lapply(Prediction_Ligandability, is.numeric))], 3)
  
# Prediction
Prediction <- Prediction_Ligandability %>%
  dplyr::select(c(1:2)) %>%
  left_join(., Feature_info,
            by = "Gene")

Prediction_Ligandability <- Prediction_Ligandability %>%
  left_join(., Feature_info %>%
              dplyr::select(c(1:2)),
            by = "Gene")

# E2_Accessibility
E2_table <- read_tsv("./Data/Lysine_E2_Accessibility.txt")


