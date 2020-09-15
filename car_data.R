getwd() 
setwd("D:\\Data mining and DB programming\\datasets")
# get data
fuzzycars <- read.csv2("car_data.csv")

#sample 80% rows
samp_sz = .8
#make random results reproducible
set.seed(02075049) 
train = sample(1:nrow(fuzzycars), samp_sz * nrow(fuzzycars))
# test data set is other rows
cars.train = fuzzycars[train,]
cars.test = fuzzycars[-train,]

#install.packages("tree")
library(tree)
# train a model on just the training data
tree.cars.train = tree(acceptability ~ .,cars.train)
#check predictions
cars.pred = predict(tree.cars.train,cars.test,type="class")
#visually examine diffs between "ground truth" and predictions
data.frame(cars.pred, cars.test$acceptability)
table(cars.pred, cars.test$acceptability)
summary(tree.cars.train )

summary(cars.pred)

plot(tree.cars.train)
text(tree.cars.train, pretty=0) 

cars.pred = predict(tree.cars.train,cars.test,type="class") # predictions
# confusion matrix and accuracy calc
library(caret)
confusionMatrix(cars.pred, cars.test$acceptability, mode = "prec_recall")

#visually examine diffs between "ground truth" and predictions
data.frame(cars.pred, cars.test$acceptability)

new_cars = data.frame(purchase_price = c("high"), maint_cost=  c("high"), num_doors = c(2), num_persons = c(4),trunk_size = c("small"),safety_rating = c("high"))
                      
                      
new_cars_pred = predict(tree.cars.train, new_cars,type="class")
new_cars_pred
summary(new_cars_pred)
install.packages("randomForest")
library(randomForest)
set.seed(02075049)
# build random forest and track variable importance
rf.cars=randomForest(acceptability~.,data = cars.train,importance=TRUE)
rf.cars

# show importance
importance(rf.cars)
varImpPlot(rf.cars)
# predict on test set
rf.cars.pred=predict(rf.cars,cars.test,type="class")
# confusion matrix and accuracy calc
library(caret)
confusionMatrix(rf.cars.pred, cars.test$acceptability, mode = "prec_recall")



