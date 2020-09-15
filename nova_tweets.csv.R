getwd() 
setwd("D:\\Data mining and DB programming\\datasets")
# get data
novatweets <- read.csv("nova_tweets.csv")

novatweets$tod<-ifelse(novatweets$tod>6&novatweets$tod<20,"day","night")

# examine the structure of the sms data
str(novatweets)

# convert to factor.
novatweets$tod <- factor(novatweets$tod)

# examine the type variable more carefully
str(novatweets$tod)
table(novatweets$tod)

#to determine percentages of type of variable
prop.table(table(novatweets$tod))

# build a corpus (body of docs) using the text mining (tm) package
#install.packages("tm")
library(tm)
# VCorpus is an in-memory (volatile) corpus - simplest choice here
novatweets_corpus <- VCorpus(VectorSource(novatweets$tweet_txt))
# examine the sms corpus
print(novatweets_corpus)
inspect(novatweets_corpus[1:2])
# see contents
as.character(novatweets_corpus[[1]])
# returns a list in which the function has been applied to each item
lapply(novatweets_corpus[1:2], as.character)

# clean up the corpus using tm_map()
# start by making everything lowercase

novatweets_corpus_clean <- tm_map(novatweets_corpus, content_transformer(tolower))
# show the difference between sms_corpus and corpus_clean
as.character(novatweets_corpus[[1]])
as.character(novatweets_corpus_clean[[1]])
# remove numbers
novatweets_corpus_clean <- tm_map(novatweets_corpus_clean, removeNumbers) 
# remove stop words
novatweets_corpus_clean <- tm_map(novatweets_corpus_clean, removeWords, stopwords())
# remove punctuation
novatweets_corpus_clean <- tm_map(novatweets_corpus_clean, removePunctuation)

# illustration of word stemming
#install.packages("SnowballC")
library(SnowballC)
# now stem our actual corpus
novatweets_corpus_clean <- tm_map(novatweets_corpus_clean, stemDocument)
# eliminate unneeded whitespace
novatweets_corpus_clean <- tm_map(novatweets_corpus_clean, stripWhitespace) 
# examine the final clean corpus
lapply(novatweets_corpus[1:3], as.character)
lapply(novatweets_corpus_clean[1:3], as.character)


# create a document-term sparse matrix
novatweets_dtm <- DocumentTermMatrix(novatweets_corpus_clean)
# see the result
novatweets_dtm
# creating training and test datasets
train_pct <- .8
set.seed(02075049)
train = sample(1:nrow(novatweets_dtm),train_pct * nrow(novatweets_dtm))
novatweets_dtm_train <- novatweets_dtm[train, ]
novatweets_dtm_test  <- novatweets_dtm[-train, ]
# also save the labels
novatweets_train_labels <- novatweets[train, ]$tod
novatweets_test_labels  <- novatweets[-train, ]$tod
# check that the proportion of spam is similar
prop.table(table(novatweets_train_labels))
prop.table(table(novatweets_test_labels))

# subset the training data into spam and ham groups
day <- subset(novatweets, tod == "day")
night  <- subset(novatweets, tod == "night")
# word cloud visualization
#install.packages("wordcloud")
library(wordcloud)
wordcloud(spam$tweet_txt, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$tweet_txt, max.words = 40, scale = c(3, 0.5))


# increase print output
# options(max.print = 999999)
# indicator features for frequent words
findFreqTerms(novatweets_dtm_train, 5)
# save frequently-appearing terms to a character vector
novatweets_freq_words <- findFreqTerms(novatweets_dtm_train, 5)
str(novatweets_freq_words)
# create DTMs with only the frequent terms
novatweets_dtm_freq_train <- novatweets_dtm_train[ , novatweets_freq_words]
novatweets_dtm_freq_test <- novatweets_dtm_test[ , novatweets_freq_words]
#see what is in a DTM
inspect(novatweets_dtm_freq_train)
as.character(novatweets_corpus_clean[[2360]])
as.character(novatweets_corpus[[2360]])
# convert counts to presence indicator - # times not important
convert_counts <- function(x) {
  return(ifelse(x > 0, "Yes", "No"))
}
# apply() convert_counts() to columns of train/test data
novatweets_train <- apply(novatweets_dtm_freq_train, MARGIN = 2, convert_counts)
novatweets_test  <- apply(novatweets_dtm_freq_test, MARGIN = 2, convert_counts)


# Training a model on the data ----
#install.packages("e1071")
library(e1071)
novatweets_classifier <- naiveBayes(novatweets_train, novatweets_train_labels)
#Evaluating model performance ----
novatweets_test_pred <- predict(novatweets_classifier, novatweets_test)
novatweets_test_pred
library(caret)
confusionMatrix(novatweets_test_pred, novatweets_test_labels, mode = "prec_recall", positive = "night" )

