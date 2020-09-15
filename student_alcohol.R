getwd() 
setwd("D:\\Data mining and DB programming\\datasets")
# get data
student_alcohol_raw <- read.csv("student_alcohol.csv")
#cleaning data by removing columns which are not considered for prediction
student_alcohol<-subset(student_alcohol_raw,select=-c(famsize, Pstatus, Medu, Fedu, Mjob,  Fjob, reason, guardian, traveltime,
              studytime, schoolsup, famsup, paid, activities, nursery, higher, internet, romantic,health, studentid))

# convert to factor.
#student_alcohol$Walc <- factor(student_alcohol$Walc)

summary(student_alcohol$Walc)

recode <- c("No" = 1,"No" = 2,"No" = 3,"Yes" = 4,"Yes"=5)

student_alcohol$Walc <- factor(student_alcohol$Walc, levels=recode, 	labels=names(recode))
summary(student_alcohol$Walc)

#create a linear model
lm.all_predictors <- lm( Walc ~ ., data = student_alcohol)
summary(lm.all_predictors)

# creating training and test datasets
train_pct <- .8
set.seed(02075049)
train = sample(1:nrow(student_alcohol),train_pct * nrow(student_alcohol))
student_alcohol_train <- student_alcohol[train, ]
student_alcohol_test  <- student_alcohol[-train, ]

summary(student_alcohol_train)

#logistic regression
# percentage for training data
train_pct <- 0.7
# seed random number generator to make results reproducible
set.seed(54321)
student_alcohol_train_rows <- sample(1:nrow(student_alcohol),
                              train_pct * nrow(student_alcohol))
student_alcohol_train_rows
# create training and test sets
student_alcohol_train <- student_alcohol[student_alcohol_train_rows,]
student_alcohol_test <- student_alcohol[-student_alcohol_train_rows,]

# build logistic regression model on training data
glm_fit <- glm(Walc~., data=student_alcohol_train, family=binomial)
summary(glm_fit)
# now make predictions on test data
# set threshold for predicting target class of "Yes"
target_threshold <- .5
# get probability for each test instance
glm_fit_probs <- predict(glm_fit,type="response", 
                         newdata = student_alcohol_test)
# create vector of predictions - say "Yes" when over threshold
glm_fit_preds <- rep("No", nrow(student_alcohol_test))
glm_fit_preds[glm_fit_probs > target_threshold]="Yes"
glm_fit_preds

confusionMatrix(as.factor(glm_fit_preds),student_alcohol_test$Walc,	positive = "Yes", mode = "prec_recall")





