getwd() 
setwd("D:\\Data mining and DB programming\\datasets")
# get data
movies<- read.csv("imdb.csv")
movies_clean <- movies

recode <- c("No" = 0, "Yes" = 1)
movies_clean$imdb_score_high <- factor(movies_clean$imdb_score_high, levels=recode, 	labels=names(recode))
# percentage for training data
train_pct  <- 0.8
# seed random number generator to make results reproducible
set.seed(02075049)
movies_train_rows <- sample(1:nrow(movies_clean),
                              train_pct * nrow(movies_clean))

# create training and test sets
movies_train <- movies_clean[movies_train_rows,]
movies_test <- movies_clean[-movies_train_rows,]
summary(movies_train)

# build logistic regression model on training data
glm_fit_movie <- glm(imdb_score_high~., data=movies_train, family=binomial)
summary(glm_fit_movie)

# now make predictions on test data
# set threshold for predicting target class of "Yes"
target_threshold <- .3
# get probability for each test instance
glm_fit_movie_probs <- predict(glm_fit_movie,type="response", 
                         newdata = movies_test)
# create vector of predictions - say "Yes" when over threshold
glm_fit_movie_preds <- rep("No", nrow(movies_test))
glm_fit_movie_preds[glm_fit_movie_probs > target_threshold]="Yes"
glm_fit_movie_preds



# generate confusion matrix
#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)
confusionMatrix(as.factor(glm_fit_movie_preds),movies_test$imdb_score_high,	positive = "Yes", mode = "prec_recall")

# create ROC curve for model
#install.packages("pROC")
library(pROC)
roc_obj <- roc(movies_test$imdb_score_high, glm_fit_movie_probs)
# see AUC - area under curve
roc_obj$auc
# draw ROC curve with title and AUC
#install.packages("ggfortify")
#library(ggfortify)
roc_plot <- ggroc(roc_obj, legacy.axes = TRUE)
roc_plot + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),    
               color="darkgrey", linetype="dashed") + ggtitle(paste("ROC Curve with AUC",roc_obj$auc)) 

predict(glm_fit_movie, data.frame(num_critic_for_reviews=c(97), duration=c(110), director_facebook_likes=c(54), lead_actor_facebook_likes=c(1137), gross =c(14681875), 	num_voted_users	=c(18585), cast_total_facebook_likes	= c(3165),
                            
                            facenumber_in_poster =c(1),num_user_for_reviews=c(37),budget=c(25000000),title_year=(2014),aspect_ratio=c(4.35),movie_facebook_likes=c(19000)),type="response") * 100

