getwd()
setwd("D:\\Data mining and DB programming\\datasets")

library(tree)
income = read.csv("census_income.csv")
income_clean = income
# remove questionable variable
income_clean = income_clean[,-7]
summary(income_clean)
# create train and test sets to build and check model
set.seed(123)
sample_pct <- .80
train <- sample(1:nrow(income), sample_pct * nrow(income))
income.train <- income_clean[train,] #train set
income.test <- income_clean[-train,] #test set
tree.income<-tree(over_50k~.,income.train)
summary(tree.income)
plot(tree.income)
text(tree.income, pretty=0) # pretty uses factor names for plot
tree.pred=predict(tree.income,income.test,type="class") # predictions
# confusion matrix and accuracy calc
library(caret)
confusionMatrix(tree.pred, income.test$over_50k, mode = "prec_recall", positive="Yes")



#install.packages("randomForest")
library(randomForest)
set.seed(123)
# build random forest and track variable importance
rf.income=randomForest(over_50k~.,data=income.train,importance=TRUE)
rf.income
# show importance
importance(rf.income)
varImpPlot(rf.income)
# predict on test set
rf.income.pred=predict(rf.income,income.test,type="class")
# confusion matrix and accuracy calc
library(caret)
confusionMatrix(rf.income.pred, income.test$over_50k, mode = "prec_recall", positive="Yes")
positive="Yes"
confusionMatrix(cars.pred, cars.test$acceptability, mode = "prec_recall", positive="Yes")




# need our target to be ones and zeroes
income.train.forboost = income.train
income.train.forboost$over_50k = ifelse(income.train.forboost$over_50k=="Yes",1,0)
income.test.forboost = income.test
income.test.forboost$over_50k = ifelse(income.test.forboost$over_50k=="Yes",1,0)



install.packages("gbm")
library(gbm)
set.seed(123)
# create a boosted model with 1000 piggybacked trees - verbose lets us see interim results
# Bernoulli distribution good for our 0 and 1 values in the target
boosted.income = gbm(over_50k~.,data=income.train.forboost,n.trees=1000, distribution = "bernoulli", verbose=TRUE)
boosted.income
summary(boosted.income)
# make predictions
boosted.pred = predict(boosted.income,income.test.forboost, n.trees=1000, type="response")
# turn probabilities to text values
boosted.pred = ifelse(boosted.pred > 0.5,"Yes","No")
# confusion matrix and accuracy calc
confusionMatrix(as.factor(boosted.pred), income.test$over_50k, mode = "prec_recall", positive="Yes")



























