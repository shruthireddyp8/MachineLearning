getwd()
setwd("D:\\Data mining and DB programming\\datasets")
# read the sms data into the sms data frame
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
# examine the structure of the sms data
str(sms_raw)
# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)
# examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)
#to determine percentages of type of variable
prop.table(table(sms_raw$type))
# build a corpus (body of docs) using the text mining (tm) package
#install.packages("tm")
library(tm)
# VCorpus is an in-memory (volatile) corpus - simplest choice here
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
# examine the sms corpus
print(sms_corpus)
inspect(sms_corpus[1:2])
# see contents
as.character(sms_corpus[[1]])
# returns a list in which the function has been applied to each item
lapply(sms_corpus[1:2], as.character)

# clean up the corpus using tm_map()
# start by making everything lowercase
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
# show the difference between sms_corpus and corpus_clean
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
# remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) 
# remove stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
stopwords()
#to remove all the stop words list followed by comcast use below function 
c(comcast,stopwords())
# remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
# illustration of word stemming
#install.packages("SnowballC")
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))
# now stem our actual corpus
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
# eliminate unneeded whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) 
# examine the final clean corpus
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
# see the result
sms_dtm
inspect(sms_dtm)
# creating training and test datasets
train_pct <- .8
set.seed(123)
train = sample(1:nrow(sms_dtm),train_pct * nrow(sms_dtm))
sms_dtm_train <- sms_dtm[train, ]
sms_dtm_test  <- sms_dtm[-train, ]
# also save the labels
sms_train_labels <- sms_raw[train, ]$type
sms_test_labels  <- sms_raw[-train, ]$type
# check that the proportion of spam is similar
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
# subset the training data into spam and ham groups
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")
# word cloud visualization
#install.packages("wordcloud")
library(wordcloud)
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


# increase print output
# options(max.print = 999999)
# indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)
# save frequently-appearing terms to a character vector
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
# create DTMs with only the frequent terms
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]
#see what is in a DTM
inspect(sms_dtm_freq_train)
as.character(sms_corpus_clean[[313]])
as.character(sms_corpus[[313]])
# convert counts to presence indicator - # times not important
convert_counts <- function(x) {
  return(ifelse(x > 0, "Yes", "No"))
}
# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)


# Training a model on the data ----
#install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
#Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)
library(caret)
confusionMatrix(sms_test_pred,sms_test_labels,positive = "spam", 
                mode = "prec_recall")




new_sms1 = 'what time is the movie? 7:30?'
new_sms2 = 'Free vacation if you text back'
# put both into same character vector
new_sms <- rbind(new_sms1, new_sms2)
new_corpus <- VCorpus(VectorSource(new_sms))
new_corpus = tm_map(new_corpus, content_transformer(tolower))
new_corpus = tm_map(new_corpus, removeNumbers)
new_corpus = tm_map(new_corpus, removePunctuation)
new_corpus = tm_map(new_corpus, removeWords, stopwords())
new_corpus = tm_map(new_corpus, stripWhitespace)
as.character(new_corpus[[1]])
new_dtm <- DocumentTermMatrix(new_corpus)
new_dtm
inspect(new_dtm)
new_dtm <- apply(new_dtm, MARGIN = 2, convert_counts)
new_sms_pred <- predict(sms_classifier, new_dtm)
new_sms_pred
# see predictions as probabilities
new_sms_pred <- predict(sms_classifier, new_dtm, type="raw")
new_sms_pred






















































