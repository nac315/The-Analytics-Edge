#Loading Libraries
library(caTools)
library(dplyr)
library(caret)
library(ggcorrplot)
library(tm)
library(RColorBrewer)
library(SnowballC)
library(rpart)
library(rpart.plot)
library(ROCR)
library(ggplot2)
library(pROC)
library(glmnet)
library(flexclust)
library(umap)
library(cluster)

##Loading Dataset
obesity_SA = read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
head(obesity_SA)

##Adding an indicator column for obesity status
obesity_SA$obese_ind = ifelse(obesity_SA$NObeyesdad %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"), 1, 0)

cor_data= subset(obesity_SA, )

#Viewing the data to make sure the column worked
head(obesity_SA)
tail(obesity_SA)

# Do train/test split
set.seed(10, sample.kind = "Rejection")
idx <- createDataPartition(obesity_SA$obese_ind, p = 0.7, list = FALSE)
train <- obesity_SA[idx,1:8]
test <- obesity_SA[-idx,1:8]

#Comparing some summary stats across the data sets
summary(train)
summary(test)


