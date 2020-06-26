library(ROSE)
library(rpart)
library(caret)
library(dplyr)

data(hacide)
str(hacide.train)
prop.table(table(hacide.train$cls))
prop.table(table(hacide.test$cls))

# Model according to the book

tree.imb <- rpart(cls ~., data = hacide.train)
pred.tree.imb <- predict(tree.imb, hacide.test)

accuracy.meas(hacide.test$cls, pred.tree.imb[,2])
roc.curve(hacide.test$cls, pred.tree.imb[,2], plotit = F)

data_rose <- ROSE(cls ~., data = hacide.train, seed = 1)$data
prop.table(table(data_rose$cls))

tree.rose <- rpart(cls ~., data = data_rose)
pred.tree.rose <- predict(tree.rose, hacide.test)

accuracy.meas(hacide.test$cls, pred.tree.rose[,2])
roc.curve(hacide.test$cls, pred.tree.rose[,2], plotit = F)

# Make confusion matrix to compare with caret package

pred_tree <- pred.tree.imb
pred_tree <- data.frame(pred_tree)
colnames(pred_tree) <- c("one", "two")
pred_tree$cls <- ifelse(pred_tree$two > 0.5, "1", "0")
pred_tree$cls <- as.factor(pred_tree$cls)

tree_imb_matrix <- confusionMatrix(pred_tree$cls, hacide.test$cls, positive = "1")
print(tree_imb_matrix)

pred_rose <- pred.tree.rose
pred_rose <- data.frame(pred_rose)
colnames(pred_rose) <- c("one", "two")
pred_rose$cls <- ifelse(pred_rose$two > 0.5, "1", "0")
pred_rose$cls <- as.factor(pred_rose$cls)

tree_rose_matrix <- confusionMatrix(pred_rose$cls, hacide.test$cls, positive = "1")
print(tree_rose_matrix)

## AUC of 0.5 is bad = random classifier
## AUC of 0.5 - 0.7 = poor classifier
## AUC of 0.7 - 0.8 = acceptable classifier
## AUC of 0.8 - 0.9 = excellent classifier
## AUC of 0.9 - 1.0 = outstanding classifier

# Model using caret package

ctrl_lm <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

hacide.train$cls <- plyr::revalue(hacide.train$cls, c("0" = "no", "1" = "yes"))
hacide.test$cls <- plyr::revalue(hacide.test$cls, c("0" = "no", "1" = "yes"))

tree.fit <- train(cls ~.,
                   data = hacide.train,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl_lm)

pred.tree.fit <- predict(tree.fit, hacide.test)
accuracy.meas(hacide.test$cls, pred.tree.fit)
roc.curve(hacide.test$cls, pred.tree.fit, plotit = FALSE)

data_rose$cls <- plyr::revalue(data_rose$cls, c("0" = "no", "1" = "yes"))

tree.bal <- train(cls ~.,
                  data = data_rose,
                  method = "rpart",
                  metric = "ROC",
                  trControl = ctrl_lm)

pred.tree.bal <- predict(tree.bal, hacide.test)
accuracy.meas(hacide.test$cls, pred.tree.bal)
roc.curve(hacide.test$cls, pred.tree.bal, plotit = FALSE)

tree_fit_matrix <- confusionMatrix(pred.tree.fit, 
                                   hacide.test$cls, positive = "yes")
print(tree_fit_matrix)

tree_bal_matrix <- confusionMatrix(pred.tree.bal, 
                                   hacide.test$cls, positive = "yes")
print(tree_bal_matrix)

# Try model tuning with train control parameters

## change the number of repeats on cross validation

ctrl_par <- trainControl(method = "cv",
                        number = 20,
                        # repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

tree.bal <- train(cls ~.,
                  data = data_rose,
                  method = "rpart",
                  metric = "ROC",
                  trControl = ctrl_par)

pred.tree.bal <- predict(tree.bal, hacide.test)
accuracy.meas(hacide.test$cls, pred.tree.bal)
roc.curve(hacide.test$cls, pred.tree.bal, plotit = FALSE)

tree_bal_matrix <- confusionMatrix(pred.tree.bal, 
                                   hacide.test$cls, positive = "yes")
print(tree_bal_matrix)

# Test rpart with different parameters

set.seed(1)

mod_rpart <- train(cls ~.,
                   method = "rpart",
                   data = data_rose,
                   tuneLength = 50,
                   metric = "ROC",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 4,
                                            repeats = 5,
                                            summaryFunction = twoClassSummary,
                                            classProbs = TRUE))

pred_part <- predict(mod_rpart, hacide.test)
confusionMatrix(pred_part, hacide.test$cls, positive = "yes")

modFitDecTree <- rpart(cls ~.,
                       data = data_rose,
                       method = "class",
                       control = rpart.control(cp = mod_rpart$bestTune))

predictDecTree <- predict(modFitDecTree, newdata = hacide.test, type = "class" )
confusionMatrix(predictDecTree, hacide.test$cls, positive = "yes")

roc.curve(hacide.test$cls, pred_part, plotit = FALSE)
roc.curve(hacide.test$cls, predictDecTree, plotit = FALSE)