# read in data and examine structure
concrete <- read.csv("concrete.csv")
str(concrete)
# apply normalization to entire data frame
# NN will work best when data normalized in range
# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
# confirm that the range is now between zero and one
summary(concrete$strength)
summary(concrete_norm$strength)
# create training and test data
train_pct <- 0.75
set.seed(123)
train <- sample(1:nrow(concrete_norm),train_pct * nrow(concrete_norm))
concrete_train <- concrete_norm[train, ]
concrete_test <- concrete_norm[-train, ]

#install.packages("neuralnet")
library(neuralnet)
# simple ANN with only a single hidden neuron
net <- neuralnet(strength~.,concrete_train,hidden=1)
plot(net)
# obtain model results
net_results <- compute(net, concrete_test[,1:8])
# obtain predicted strength values
predicted_strength <- net_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

# a more complex neural network topology with 5 hidden neurons
net2 <- neuralnet(strength~.,concrete_train,hidden=5)
plot(net2)
# obtain model results
net_results2 <- compute(net2, concrete_test[,1:8])
# obtain predicted strength values
predicted_strength2 <- net_results2$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength2, concrete_test$strength)


lm.fit = lm(strength~.,concrete_train)
summary(lm.fit)
lm_preds = predict(lm.fit,concrete_test[,1:8])
cor(lm_preds, concrete_test$strength)

library(tree)
tree.fit = tree(strength~.,concrete_train)
summary(tree.fit)
plot(tree.fit)
text(tree.fit,pretty=0)
tree_preds = predict(lm.fit,concrete_test[,1:8])
cor(tree_preds, concrete_test$strength)

library(randomForest)
rf=randomForest(strength~.,data=concrete_train,importance=TRUE)
rf
importance(rf)
rf_preds = predict(rf,concrete_test[,1:8])
cor(rf_preds, concrete_test$strength)





















