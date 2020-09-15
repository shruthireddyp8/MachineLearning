getwd()
setwd("D:\\Data mining and DB programming\\datasets")
View(iris)

#install.packages("tree")
library(tree)
# predict Species from all other attributes
tree.irises = tree(Species ~ .,iris)
summary(tree.irises)
# write tree structure to a file
sink('iristree.txt')
tree.irises
sink()
# draw the tree
plot(tree.irises)
text(tree.irises)
#try with training and test sets
#sample 80% rows
samp_sz = .8
#make random results reproducible
set.seed(123) #any number will do
train = sample(1:nrow(iris), samp_sz * nrow(iris))
# test data set is other rows
iris.train = iris[train,]
iris.test = iris[-train,]
# train a model on just the training data
tree.iris.train = tree(Species ~ .,iris.train)
#check predictions
iris.pred = predict(tree.iris.train,iris.test,type="class")
#visually examine diffs between "ground truth" and predictions
irisdf = data.frame(iris.pred, iris.test$Species)
irisdf
#confusion matrix
library(caret)
confusionMatrix(iris.pred, iris.test$Species, mode = "prec_recall" )
#predict a new example
new_iris = data.frame(Petal.Length=c(2.5), Petal.Width=c(1.5), Sepal.Length=c(2.4), Sepal.Width=c(1.7))
new_iris_pred = predict(tree.iris.train,new_iris,type="class")
new_iris_pred
