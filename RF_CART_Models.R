
# remove age,weight,height,gender AND  NObeyesdad
obesity_SA <- obesity_SA %>%
  select(-Age, -Weight, -Height, -Gender, -NObeyesdad)

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

