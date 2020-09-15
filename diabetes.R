
getwd() 
setwd("D:\\Data mining and DB programming\\datasets")

# get data
diabetes_raw <- read.csv("diabetes.csv")
# inspect it
summary(diabetes_raw)
# investigate target variable
table(diabetes_raw$Outcome)
summary(diabetes_raw$Outcome)
prop.table(table(diabetes_raw$Outcome))

#install.packages("corrgram")
library(corrgram)
# create correlogram
corrgram(diabetes_raw,cex.labels = 1.2)
# fancier one with correlation coefficients
corrgram(diabetes_raw, lower.panel=panel.shade, upper.panel=panel.cor, 
         text.panel=panel.txt, main="Diabetes Data", cex.labels = 1.2)

# going to modify raw data so make new data frame
diabetes_clean <- diabetes_raw
# will recode Outcome from 0 and 1 to Yes/No factor
recode <- c("No" = 0, "Yes" = 1)
diabetes_clean$Outcome <- factor(diabetes_clean$Outcome, levels=recode, 	labels=names(recode))
# see results of change
summary(diabetes_raw$Outcome)
summary(diabetes_clean$Outcome)

# percentage for training data
train_pct <- 0.7
# seed random number generator to make results reproducible
set.seed(54321)
diabetes_train_rows <- sample(1:nrow(diabetes_clean),
                              train_pct * nrow(diabetes_clean))
diabetes_train_rows
# create training and test sets
diabetes_train <- diabetes_clean[diabetes_train_rows,]
diabetes_test <- diabetes_clean[-diabetes_train_rows,]

# build logistic regression model on training data
glm_fit <- glm(Outcome~., data=diabetes_train, family=binomial)
summary(glm_fit)
# now make predictions on test data
# set threshold for predicting target class of "Yes"
target_threshold <- .5
# get probability for each test instance
glm_fit_probs <- predict(glm_fit,type="response", 
                         newdata = diabetes_test)
# create vector of predictions - say "Yes" when over threshold
glm_fit_preds <- rep("No", nrow(diabetes_test))
glm_fit_preds[glm_fit_probs > target_threshold]="Yes"
glm_fit_preds


confusionMatrix(as.factor(glm_fit_preds),diabetes_test$Outcome,	positive = "Yes", mode = "prec_recall")

