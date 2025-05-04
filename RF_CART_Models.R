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
obesity_SA = read.csv(file.choose())
head(obesity_SA)

##Adding an indicator column for obesity status
obesity_SA$obese_ind = ifelse(obesity_SA$NObeyesdad %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"), 1, 0)

# remove age,weight,height,gender 
obesity_SA <- obesity_SA %>%
  select(-Age, -Weight, -Height, -Gender, -NObeyesdad)

#Turning categorical variables into factors
obesity_SA$family_history_with_overweight = as.factor(obesity_SA$family_history_with_overweight)
obesity_SA$CALC = as.factor(obesity_SA$CALC)
obesity_SA$MTRANS = as.factor(obesity_SA$MTRANS)
obesity_SA$CAEC = as.factor((obesity_SA$CAEC))
obesity_SA$obese_ind = as.factor(obesity_SA$obese_ind)
obesity_SA$SMOKE = as.factor(obesity_SA$SMOKE)
obesity_SA$SCC = as.factor(obesity_SA$SCC)
obesity_SA$FAVC = as.factor(obesity_SA$FAVC)

#Viewing the data to make sure the column worked
head(obesity_SA)
tail(obesity_SA)

## Data Exploration
# Barplot for obesity by smoking status
ggplot(obesity_SA, aes(x = SMOKE, fill = obese_ind)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("lightblue", "red")) + 
  labs(title = "Obesity by Smoking Status", 
       x = "Smoking Status", 
       y = "Proportion", 
       fill = "Obesity Status") +
  theme_minimal()

# Barplot for obesity by frequency of high-calorie food consumption
ggplot(obesity_SA, aes(x = FAVC, fill = obese_ind)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("lightblue", "red")) + 
  labs(title = "Obesity by Frequency of High-Calorie Food Consumption", 
       x = "Frequency of High-Calorie Food Consumption", 
       y = "Proportion", 
       fill = "Obesity Status") +
  theme_minimal()

# Barplot for obesity by mode of transportation
ggplot(obesity_SA, aes(x = MTRANS, fill = obese_ind)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("lightblue", "red")) + 
  labs(title = "Obesity by Mode of Transportation", 
       x = "Mode of Transportation", 
       y = "Proportion", 
       fill = "Obesity Status") +
  theme_minimal()

# Barplot for obesity by family history of overweight
ggplot(obesity_SA, aes(x = family_history_with_overweight, fill = obese_ind)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("lightblue", "red")) + 
  labs(title = "Obesity by Family History of Overweight", 
       x = "Family History of Overweight", 
       y = "Proportion", 
       fill = "Obesity Status") +
  theme_minimal()


# Do train/test split
#Creating a interaction term to stratify smaple smoking and SCC
obesity_SA$SamplingTerm <- interaction(obesity_SA$SMOKE,obesity_SA$SCC)

set.seed(10, sample.kind = "Rejection")
idx <- createDataPartition(obesity_SA$SamplingTerm, p = 0.7, list = FALSE)

train <- obesity_SA[idx,]
test <- obesity_SA[-idx, ]

#Comparing some summary stats across the data sets
summary(train)
summary(test)

##CART MODEL
# Perform 8-fold cross-validation
set.seed(314)
cv.trees <- train(
  y = train$obese_ind,
  x = subset(train, select = -c(obese_ind)),
  method = "rpart",
  trControl = trainControl(method = "cv", number = 8),
  tuneGrid = data.frame(.cp = seq(0.001, 0.01, 0.0001))
)

# Plot cp vs. average cross-validation accuracy
plot(
  cv.trees$results$cp, 
  cv.trees$results$Accuracy, 
  xlab = "cp", 
  ylab = "Accuracy",
  main = "Cross-validation Accuracy vs Complexity Parameter (cp)"
)
# Print the single CP value achieving the best accuracy
cv.trees$bestTune$cp

#Train a CART model with the best cp value) 
model.tree = rpart(obese_ind ~ ., data=train, control = rpart.control(cp=(0.0016)))

# Plot the tree with prp function
prp(model.tree, digits=2, extra=107, type=2)

### Compute predicted probabilities for the test set
tree.pred <- predict(model.tree, newdata = test)[,2]

### Create a prediction object for ROC analysis
rocr.pred <- prediction(tree.pred,test$obese_ind)


# Plot the ROC curve. Color is blue and lty=1 makes it solid.
plot(performance(rocr.pred, "tpr", "fpr"), col = "blue", lty = 1, main = "ROC Curve for CART Model")

# Plot the baseline function (a line y = 0 + 1 * x). Color is red and lty=2 makes it dashed. 
abline(0, 1, col = "red", lty = 2)

# Add a legend in the bottom right corner
legend("bottomright", 
       legend = c("CART", "Baseline"), 
       col = c("blue", "red"), 
       lty = 1:2, 
       cex = 0.7)
# Compute and print the AUC value
AUC <- as.numeric(performance(rocr.pred, "auc")@y.values)
print(AUC)

# Variable importance plot
CART_importance_scores <- model.tree$variable.importance
n_variables <- 10 
barplot(
  tail(sort(CART_importance_scores), n_variables),
  beside = TRUE,
  horiz = TRUE,
  las = 1,
  main = paste("CART - Top", n_variables, "Importance Scores"),
  cex.names = 0.7
)

### Compute the confusion matrix

tree.pred <- predict(model.tree, newdata = test, type = "class")

cm <- confusionMatrix(factor(tree.pred), factor(test$obese_ind))
cm

## Random Forest Model
### Train a Random Forest model using oob evaluation
train.rf.oob <- train(
  y = train$obese_ind,
  x = subset(train, select = -c(obese_ind)),
  method = "rf",
  ntree = 350,
  nodesize = (25),
  tuneGrid = data.frame(mtry = seq(20, 30, 2)),
  trControl = trainControl(method = "oob")
)
#
train.rf.oob

### Create a barplot of the top variable importance scores
best.rf <- train.rf.oob$finalModel 
RF_importance_scores <- best.rf$importance[, 1]
n_variables <- 10  
barplot(
  tail(sort(RF_importance_scores), n_variables),
  beside = TRUE,
  horiz = TRUE,
  las = 1,
  main = paste("Random Forest - Top", n_variables, "Importance Scores"),
  cex.names = 0.7
)
##Performance 

rf.pred.prob <- predict(train.rf.oob, newdata = test, type = "prob")
rf.pred.class <- predict(train.rf.oob, newdata = test, type = "raw")

# Compute the ROC curve and AUC
rf.rocr.pred <- prediction(rf.pred.prob[, 2], test$obese_ind)

# Plot ROC curve
plot(performance(rf.rocr.pred, "tpr", "fpr"), col = "blue", lty = 1, main = "ROC Curve for Random Forest")
abline(0, 1, col = "red", lty = 2)  # Baseline (random guessing)
legend("bottomright", legend = c("Random Forest", "Baseline"), col = c("blue", "red"), lty = 1:2, cex = 0.7)

# Compute and print AUC value
rf.auc <- as.numeric(performance(rf.rocr.pred, "auc")@y.values)
print(paste("AUC:", round(rf.auc, 4)))

# Generate Confusion Matrix for Random Forest
library(caret)
cm_rf <- confusionMatrix(factor(rf.pred.class), factor(test$obese_ind))

# Print confusion matrix
print(cm_rf)


###LOSS MATRIX 
# Create a 2x2 loss matrix, penalizing False Negatives more
LossMatrix <- matrix(0, 2, 2)
LossMatrix[2, 1] <- 5  # Higher penalty for False Negatives
LossMatrix[1, 2] <- 1  # Lower penalty for False Positives

# Perform 8-fold cross-validation
cv.trees <- train(
  y = train$obese_ind,
  x = subset(train, select = -c(obese_ind)),
  method = "rpart",
  parms = list(loss = LossMatrix),  # Include custom loss matrix
  trControl = trainControl(method = "cv", number = 8),
  tuneGrid = data.frame(.cp = seq(0.001, 0.01, 0.0001))
)

#Plot cp vs. average cross-validation accuracy
plot(
  cv.trees$results$cp, 
  cv.trees$results$Accuracy, 
  xlab = "cp", 
  ylab = "Accuracy",
  main = "Cross-validation Accuracy vs Complexity Parameter (cp)"
)

# best cp 
cv.trees$bestTune$cp

#Train a CART model with the best cp value) 
model.tree2 = rpart(obese_ind ~ ., data=train, control = rpart.control(cp=(0.0043)))


# Plot the tree with prp function
prp(model.tree2, digits=2, extra=107, type=2)

#Compute predicted probabilities for the test set
tree.pred <- predict(model.tree2, newdata = test)[,2]

#Create a prediction object for ROC analysis) ###
rocr.pred <- prediction(tree.pred,test$obese_ind)

# Plot the ROC curve. Color is blue and lty=1 makes it solid.
plot(performance(rocr.pred, "tpr", "fpr"), col = "blue", lty = 1, main = "ROC Curve for CART Model")
# Plot the baseline function (a line y = 0 + 1 * x). Color is red and lty=2 makes it dashed. 
abline(0, 1, col = "red", lty = 2)
legend("bottomright", 
       legend = c("CART", "Baseline"), 
       col = c("blue", "red"), 
       lty = 1:2, 
       cex = 0.7)

# Compute and print the AUC value
AUC <- as.numeric(performance(rocr.pred, "auc")@y.values)
print(AUC)

#Compute the confusion matrix

tree.pred <- predict(model.tree2, newdata = test, type = "class")
cm <- confusionMatrix(factor(tree.pred), factor(test$obese_ind))
cm
# Variable importance plot
CART_importance_scores <- model.tree2$variable.importance
n_variables <- 10 
barplot(
  tail(sort(CART_importance_scores), n_variables),
  beside = TRUE,
  horiz = TRUE,
  las = 1,
  main = paste("CART - Top", n_variables, "Importance Scores"),
  cex.names = 0.7
)

## RANDOM FOREST WITH LOSS MATRIX 
# Train a Random Forest model using out-of-bag (oob) evaluation and custom loss matrix
train.rf.oob_loss <- train(
  y = train$obese_ind,
  x = subset(train, select = -c(obese_ind)),
  method = "rf",
  ntree = 350,
  nodesize = 25,
  parms = list(loss = LossMatrix) ,
  tuneGrid = data.frame(mtry = seq(20, 30, 2)),
  trControl = trainControl(method = "oob"),
   # Include custom loss matrix
)

### Create a barplot of the top variable importance scores
best.rf <-train.rf.oob_loss$finalModel 
RF_importance_scores <- best.rf$importance[, 1]
n_variables <- 10  
barplot(
  tail(sort(RF_importance_scores), n_variables),
  beside = TRUE,
  horiz = TRUE,
  las = 1,
  main = paste("Random Forest - Top", n_variables, "Importance Scores"),
  cex.names = 0.7
)
##Performance 

rf.pred.prob <- predict(train.rf.oob_loss, newdata = test, type = "prob")
rf.pred.class <- predict(train.rf.oob_loss, newdata = test, type = "raw")

# Compute the ROC curve and AUC
rf.rocr.pred <- prediction(rf.pred.prob[, 2], test$obese_ind)

# Plot ROC curve
plot(performance(rf.rocr.pred, "tpr", "fpr"), col = "blue", lty = 1, main = "ROC Curve for Random Forest")
abline(0, 1, col = "red", lty = 2)  # Baseline (random guessing)
legend("bottomright", legend = c("Random Forest", "Baseline"), col = c("blue", "red"), lty = 1:2, cex = 0.7)

# Compute and print AUC value
rf.auc <- as.numeric(performance(rf.rocr.pred, "auc")@y.values)
print(paste("AUC:", round(rf.auc, 4)))

# Generate Confusion Matrix for Random Forest
cm_rf <- confusionMatrix(factor(rf.pred.class), factor(test$obese_ind))

# Print confusion matrix
print(cm_rf)


