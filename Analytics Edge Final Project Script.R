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

#Turning categorical variables into factors
obesity_SA$family_history_with_overweight = as.factor(obesity_SA$family_history_with_overweight)
obesity_SA$CALC = as.factor(obesity_SA$CALC)
obesity_SA$MTRANS = as.factor(obesity_SA$MTRANS)
obesity_SA$Gender = as.factor(obesity_SA$Gender)
obesity_SA$CAEC = as.factor((obesity_SA$CAEC))
obesity_SA$obese_ind = as.factor(obesity_SA$obese_ind)
obesity_SA$SMOKE = as.factor(obesity_SA$SMOKE)
obesity_SA$SCC = as.factor(obesity_SA$SCC)
obesity_SA$FAVC = as.factor(obesity_SA$FAVC)


cor_data= subset(obesity_SA, )

#Viewing the data to make sure the column worked
head(obesity_SA)
tail(obesity_SA)

# Do train/test split
#Creating a interaction term to stratify smaple smoking and SCC
obesity_SA$SamplingTerm <- interaction(obesity_SA$SMOKE,obesity_SA$SCC)

set.seed(10, sample.kind = "Rejection")
idx <- createDataPartition(obesity_SA$SamplingTerm, p = 0.7, list = FALSE)
# idx
train <- obesity_SA[idx,1:18]
test <- obesity_SA[-idx,1:18]

#Comparing some summary stats across the data sets
head(train)
summary(train)
summary(test)

obesity_SA$gender_ind <- ifelse(obesity_SA$Gender == 'Male', 1, 0)
head(obesity_SA)


# # Plot correlation matrix
# ggcorrplot(cor_mat, show.diag=FALSE, hc.order = TRUE)

#Logistic Regression
log.model = glm(obese_ind ~ . -NObeyesdad, data=train, family='binomial')

summary(log.model)






