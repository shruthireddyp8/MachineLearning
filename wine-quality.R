# read in data and examine structure
wine_quality <- read.csv("wine-quality.csv")
str(wine_quality)
# apply normalization to entire data frame
# NN will work best when data normalized in range
# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

wine_quality_norm <- as.data.frame(lapply(wine_quality, normalize))
# confirm that the range is now between zero and one
summary(wine_quality$quality)
summary(wine_quality_norm$quality)
# create training and test data
train_pct <- 0.75
set.seed(54321)
train <- sample(1:nrow(wine_quality_norm),train_pct * nrow(wine_quality_norm))
wine_quality_train <- wine_quality_norm[train, ]
wine_quality_test <- wine_quality_norm[-train, ]

#install.packages("neuralnet")
library(neuralnet)
# simple ANN with only a single hidden neuron
net <- neuralnet(quality~.,wine_quality_train,hidden=1)
plot(net)
# obtain model results
net_results <- compute(net, wine_quality_test[,1:8])
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


#install.packages("caret")
library(caret)
#decay fights overfitting
my.grid <- expand.grid(.decay = c(0.01, 0.001, 0.0001), .size = c(8, 10, 12))
caret.net.fit <- train(strength~., data = concrete_train,
                       method = "nnet", maxit = 1000, tuneGrid = my.grid, 
                       trace = FALSE)
caret.net.fit
caret.net.predict <- predict(caret.net.fit, newdata = concrete_test[,1:8])
cor(caret.net.predict, concrete_test$strength)























