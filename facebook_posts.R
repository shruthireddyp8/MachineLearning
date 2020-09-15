getwd() 
setwd("D:\\Data mining and DB programming\\datasets")
# get data
posts <- read.csv2("facebook_posts.csv")
df <- posts[grep("Photo", posts$Type),]


posts<-subset(df, select=-c(Type,Total.Interactions, Lifetime.Post.Total.Reach, Lifetime.Engaged.Users,Lifetime.Post.Consumers, Lifetime.Post.Consumptions,
                                   Lifetime.Post.Impressions.by.people.who.have.liked.your.Page, Lifetime.Post.reach.by.people.who.like.your.Page,
                                   Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post))
summary(posts)

#create a linear model
lm.all_predictors <- lm( Lifetime.Post.Total.Impressions ~ ., data = posts)
summary(lm.all_predictors)
#create a linear model ba
lm.like = lm(data = posts, Lifetime.Post.Total.Impressions ~ like)
summary(lm.like)

#plot(lm.all_predictors)
cooks_dist <-cooks.distance(lm.all_predictors)

# plot Cook's Distance
# pch means plot character
# cex is character expansion ratio
plot(cooks_dist, pch="*", cex=2, main="Influential Observations - Cook's Distance")
# add cutoff line at x times the mean of Cook's Distance
cooks_cutoff <- 4
abline(h = cooks_cutoff*mean(cooks_dist, na.rm=T), col="red") 

#identify(posts$like, posts$Lifetime.Post.Total.Impressions, paste(posts$Lifetime.Post.Total.Impressions, ' ', posts$like)

#identify(posts$like, posts$Lifetime.Post.Total.Impressions, paste(posts$Lifetime.Post.Total.Impressions, ' ', posts$like))
text(Lifetime.Post.Total.Impressions ~ , labels=rownames(posts),data=posts, cex=0.9, font=2)
#outlier row numbers
outlier <- as.numeric(names(cooks_dist)[(cooks_dist > cooks_cutoff*mean(cooks_dist, na.rm=T))])
# remove those rows
posts_rm_out  <- posts[ !(rownames(posts) %in% outlier), ]

#build new model
lm.all_predictors_rm_out  <- lm( Lifetime.Post.Total.Impressions ~ ., data = posts_rm_out)
summary(lm.all_predictors_rm_out)

lm.like_rm_out   <- lm( Lifetime.Post.Total.Impressions ~ like , data = posts_rm_out)
summary(lm.like_rm_out)

# let's see the difference with outliers removed
plot(posts_rm_out$like,posts_rm_out$Lifetime.Post.Total.Impressions)
abline(lm.like , col="red") #old model
abline(lm.like_rm_out, col="blue") #new model

# which predicts better?
# try model built on outliers
sqft_preds <- predict(lm.sqft, newdata = homes_test)
sqft_preds_avg_error <- mean(abs(sqft_preds - homes_test$price))
sqft_preds_avg_error
# now try model built without outliers
outrm_sqft_preds <- predict(lm.sqft_outrm, newdata = homes_test)
outrm__sqft_preds_avg_error = mean(abs(outrm_sqft_preds - homes_test$price))
outrm__sqft_preds_avg_error



# which predicts better?
# try model built on outliers
sqft_preds <- predict(lm.sqft, newdata = homes_test)
sqft_preds_avg_error <- mean(abs(sqft_preds - homes_test$price))
sqft_preds_avg_error
# now try model built without outliers
outrm_sqft_preds <- predict(lm.sqft_outrm, newdata = homes_test)
outrm__sqft_preds_avg_error = mean(abs(outrm_sqft_preds - homes_test$price))
outrm__sqft_preds_avg_error
data(posts)

npredict(lm.all_predictors_rm_out , data.frame(Page.total.likes=c(100000) , Category=c(1), Post.Month=c(10), 
    Post.Weekday=c(7), Post.Hour=c(13), Paid=c(1), comment=c(48),like=c(2020),share=c(144)),interval="confidence")



        