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
library(xgboost)

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
obesity_SA$NObeyesdad = as.factor(obesity_SA$NObeyesdad)


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
head(test)
summary(train)
summary(test)

# obesity_SA$gender_ind <- ifelse(obesity_SA$Gender == 'Male', 1, 0)
# head(obesity_SA)

#Calculating means of numeric variables
numeric_means <- sapply(obesity_SA, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA)

# Remove the non-numeric results (optional)
numeric_means <- numeric_means[!is.na(numeric_means)]

# View the result
print(numeric_means)

# # Plot correlation matrix
corr.matrix = cor(obesity_SA[sapply(obesity_SA, is.numeric)])
ggcorrplot(corr.matrix, show.diag=FALSE, hc.order = TRUE)

### Logistic Regression

# Attempt 1
log.model = glm(obese_ind ~ . - NObeyesdad -Weight -Height, data=train, family='binomial')

summary(log.model)


# Confusion Matrix on test data

# Test log.model has slightly higher accuracy and lower FNR than the following logistic regression models

obesity_predicted = predict(log.model, newdata=test, type="response")
obesity_predicted <- ifelse(obesity_predicted > 0.5, 1, 0)

confusionMatrix(as.factor(obesity_predicted), as.factor(test$obese_ind))


# For multicollinearity
library(car)
log.model.drop.collinear = vif(glm(obese_ind ~ . - NObeyesdad, data = train, family = "binomial"))

print(log.model.drop.collinear)

# Attempt 2-4

log_model= glm(data = train, obese_ind ~ family_history_with_overweight + FAVC + FCVC +NCP + CAEC+ SMOKE +CH2O + SCC + FAF +TUE +CALC +MTRANS, family="binomial")
summary(log_model)

log_model_2 = glm(data = train, obese_ind ~ family_history_with_overweight + FAVC + FCVC +NCP + CAEC+ SMOKE +CH2O + SCC + FAF +TUE +MTRANS, family="binomial")
summary(log_model_2)

log_model_3 = glm(data = train, obese_ind ~ family_history_with_overweight + FAVC + FCVC + CAEC+ SMOKE +CH2O + SCC + FAF +TUE +MTRANS, family="binomial")
summary(log_model_3)
#Removing FAF
log_model_4 = glm(data = train, obese_ind ~ family_history_with_overweight + FAVC + FCVC + CAEC+ SMOKE + SCC +TUE +MTRANS, family="binomial")
summary(log_model_4)
#Baseline model with all variables and tons of multi-collinearity
log_model_naive = glm(data = train, obese_ind ~ . - NObeyesdad, family="binomial")
summary(log_model_naive)
                        
                        

log_model_preds = predict(log_model, newdata=test, type='response')
log_model_preds = ifelse(log_model_preds > 0.5, 1, 0)

log_model_2_preds = predict(log_model_2, newdata=test, type='response')
log_model_2_preds = ifelse(log_model_2_preds > 0.5, 1, 0)

log_model_3_preds = predict(log_model_3, newdata=test, type='response')
log_model_3_preds = ifelse(log_model_3_preds > 0.5, 1, 0)

log_model_4_preds = predict(log_model_4, newdata=test, type='response')
log_model_4_preds = ifelse(log_model_4_preds > 0.5, 1, 0)

log_model_naive_preds = predict(log_model_naive, newdata=test, type='response')
log_model_naive_preds = ifelse(log_model_naive_preds > 0.5, 1, 0)

##Log_Model_3 removes insignificant variables and still maintains similar AUC                         

# all three yield same confusion matrix
# seem slightly worse than log.model above 
confusionMatrix(as.factor(log_model_preds), as.factor(test$obese_ind))
confusionMatrix(as.factor(log_model_2_preds), as.factor(test$obese_ind))
confusionMatrix(as.factor(log_model_3_preds), as.factor(test$obese_ind))
confusionMatrix(as.factor(log_model_4_preds), as.factor(test$obese_ind))
confusionMatrix(as.factor(log_model_naive_preds), as.factor(test$obese_ind))                        


##ROC and AUC
#Create the prediction object
rocr.pred <- prediction(log_model_preds, test$obese_ind)
rocr.pred_2 <- prediction(log_model_2_preds, test$obese_ind)
rocr.pred_3 <- prediction(log_model_3_preds, test$obese_ind)
rocr.pred_4 <- prediction(log_model_4_preds, test$obese_ind)
rocr.pred.naive <- prediction(log_model_naive_preds, test$obese_ind)

# Plot the ROC curve
plot(performance(rocr.pred, "tpr", "fpr"))
plot(performance(rocr.pred_2, "tpr", "fpr"))
plot(performance(rocr.pred_3, "tpr", "fpr"))
plot(performance(rocr.pred_4, "tpr", "fpr"))
plot(performance(rocr.pred.naive, "tpr", "fpr"))

##AUC Values
AUC <- as.numeric(performance(rocr.pred, "auc")@y.values)
AUC

AUC_2 <- as.numeric(performance(rocr.pred_2, "auc")@y.values)
AUC_2

AUC_3 <- as.numeric(performance(rocr.pred_3, "auc")@y.values)
AUC_3

AUC_4 <- as.numeric(performance(rocr.pred_4, "auc")@y.values)
AUC_4

AUC_naive <- as.numeric(performance(rocr.pred.naive, "auc")@y.values)
AUC_naive





                        

##Threshold discussion:
#Estimates for the lifetime cost of obesity in the US range from $2-3K per year (https://www.cdc.gov/obesity/adult-obesity-facts/index.html) with a lifetime cost of $92,235 (https://www.brookings.edu/wp-content/uploads/2015/05/0512-obesity-presentation-v6-rm.pdf)
#One potential treatment is bariatric surgery, which is roughly 72% effective (https://pmc.ncbi.nlm.nih.gov/articles/PMC5112115/) at helping patients lose weight, but can cost $25K
#From a preventative perspective, a simulated study found that a ten year intervention plan for children would cost ~$119 per person and reduced the proportion of the population with obesity by ~2% (https://pmc.ncbi.nlm.nih.gov/articles/PMC5654390/)

#Information is limited on how these costs evolve on a per person basis in Latin America, but we can use their relative magnitudes to identify a better probability threshold.
#For the purpose of simplicity, we'll assume with probability p a person will pay the $92K in excess medical costs
#With successful intervention at a cost of $4K, we'll assume that p will reduce by 20%

#This gives us an equation for threshold p -> 92235*p = $4K*(1-0.8p)+96235*0.8p -> p=0.218

#Confusion Matrix for p=.218 (using log_model_3)
#Accuracy decreases slightly, but FN decrease significantly!
log_model_low_thresh_preds = predict(log_model_3, newdata=test, type='response')
log_model_low_thresh_preds = ifelse(log_model_low_thresh_preds > 0.218, 1, 0)
confusionMatrix(as.factor(log_model_low_thresh_preds), as.factor(test$obese_ind))                        

##Feature Engineering
train$FAF2 = train$FAF*train$FAF
test$FAF2 = test$FAF*test$FAF

train$CH2O2 = train$CH2O*train$CH2O
test$CH2O2 = test$CH2O*test$CH2O

log_model_FE = glm(data = train, obese_ind ~ family_history_with_overweight + FAVC + FCVC + CAEC+ SMOKE +CH2O + SCC + FAF +TUE +MTRANS +FAF2 +CH2O2, family="binomial")
summary(log_model_FE)

log_model_FE_preds = predict(log_model_FE, newdata=test, type='response')
log_model_FE_preds = ifelse(log_model_FE_preds > 0.5, 1, 0)

log_model_FE_cm <- confusionMatrix(as.factor(log_model_FE_preds), as.factor(test$obese_ind))
log_model_FE_cm

log_model_FE2 = glm(data = train, obese_ind ~ family_history_with_overweight + FAVC + FCVC + CAEC+ SMOKE +CH2O + SCC + FAF +TUE +MTRANS +CH2O2, family="binomial")
summary(log_model_FE2)

log_model_FE2_preds = predict(log_model_FE2, newdata=test, type='response')
log_model_FE2_preds = ifelse(log_model_FE2_preds > 0.5, 1, 0)

log_model_FE2_cm <- confusionMatrix(as.factor(log_model_FE2_preds), as.factor(test$obese_ind))
log_model_FE2_cm








                        
### Boosted Tree Models: Gradient Boosted and XGBoost



# Preparing data for Boosted Tree models
x.train=model.matrix(obese_ind~. -NObeyesdad -Weight -Height -Age -Gender -FAF -1,data=train) # -Weight -Height -Age -Gender -FAF
y.train=train$obese_ind
x.test=model.matrix(obese_ind~. -NObeyesdad -Weight -Height -Age -Gender -FAF -1,data=test) 
y.test=test$obese_ind

# Code to calculate R^2 and OSR^2
calculate_r2 <- function(predictions) {
  return(1 - sum((predictions - as.numeric(y.train))^2) / sum((mean(as.numeric(y.train)) - as.numeric(y.train))^2))
}

calculate_osr2 <- function(predictions) {
  return(1 - sum((predictions - as.numeric(y.test))^2) / sum((mean(as.numeric(y.train)) - as.numeric(y.test))^2))
}

# print(y.train)
# 
# print(y.test)
# 
# print(test$obese_ind)

## Gradient Boosted Tree Model - Cross Validation

set.seed(15071)

# create parameter grid to search over 
gbm.grid = expand.grid(n.trees = seq(5, 10, 15),
                       interaction.depth = seq(1, 2, 1),
                       n.minobsinnode = 10,
                       shrinkage = seq(0.1, 0.3, 0.1))

# Cross validation
# gbm.cv = train(y = train$obese_ind,
#                x = subset(train, select=-c(NObeyesdad, Weight, Height, Age, Gender, FAF)),
#                method = "gbm",
#                trControl = trainControl(method="cv", number=5),
#                tuneGrid=gbm.grid,
#                verbose=0)

gbm.cv = train(y = y.train,
               x = x.train,
               method = "gbm",
               trControl = trainControl(method="cv", number=5),
               tuneGrid=gbm.grid,
               verbose=0)

# making predictions
gbm.pred = predict(gbm.cv, newdata = x.test, type="prob")
gbm.pred <- gbm.pred[, 2] # take the second column. The first column contains the probability of "0" while the second column is the probability of "1".

head(gbm.pred)

summary(gbm.pred)

confusionMatrix(as.factor(as.integer(gbm.pred > 0.5)), 
                test$obese_ind, 
                positive = "1")

gbm.cv$bestTune

## XGBoost model - Cross Validation 


set.seed(314159, sample.kind = "Rejection") 

# create the parameter grid
xgb.grid <- expand.grid(nrounds = seq(10,100,20),
                        max_depth = seq(1,3,1),
                        eta = seq(0.1,0.3,.05),
                        gamma = seq(0,60,30),
                        colsample_bytree = .6,
                        min_child_weight = 1,
                        subsample = 1)

# perform cross validation
xgb.cv <- train(y = y.train,
                x = x.train,
                method = "xgbTree",
                trControl = trainControl(method="cv", number=5),
                tuneGrid = xgb.grid,
                verbosity=0)

xgb.cv$finalModel

## XGBoost Model Performance

xgb.val.preds = as.numeric(predict(xgb.cv, newdata = x.train))
xgb.test.preds = as.numeric(predict(xgb.cv, newdata = x.test))

# print(xgb.val.preds)
# print(xgb.test.preds)

xgb.r2 = calculate_r2(xgb.val.preds)
xgb.osr2 = calculate_osr2(xgb.test.preds)

print(paste('R^2: ', xgb.r2))
print(paste('OSR^2: ', xgb.osr2))


xgb.pred = predict(xgb.cv, newdata = x.test, type='prob')
head(xgb.pred)
dim(xgb.pred)
xgb.pred <- xgb.pred[, 2] # take the second column. The first column contains the probability of "0" while the second column is the probability of "1".

head(xgb.pred)

summary(xgb.pred)

confusionMatrix(as.factor(as.integer(xgb.pred > 0.5)), 
                test$obese_ind, 
                positive = "1")


