
require(rtweet)
install.packages(rtweet)
library(rtweet)
library(tidyverse)
install.packages(here)
library(here)
library(igraph)
library(lubridate)
install.packages("rtweet")
library (rtweet)

## replace with user api key twitter details
api_key<-'XXXXX'
api_secret<-'XXXXXXXXXXXX'
api_token<-'XXXXXXXXXXXXX'

auth_setup_default()


###get Twitter ######
famous <- search_tweets("@sundarpichai", n= 200)
head(famous, n = 3)

#### show 1st 6 tweets
textdata <- head(famous$full_text)
textdata

#### show all text tweets
textdata <- famous$full_text
textdata

library(slam)
library(tm)
library(NLP)
library(textcat)# help us in terms  categorizing the text language
library(SnowballC)
tw_document <- Corpus(VectorSource(textdata))


Reviewscorpus <- VCorpus(VectorSource(textdata), readerControl = list(reader = readPlain))
stopwords()
# CLEAN THE DATA
Reviewscorpus <- tm_map(Reviewscorpus, removePunctuation)
Reviewscorpus <- tm_map(Reviewscorpus, removeNumbers)
Reviewscorpus <- tm_map(Reviewscorpus, content_transformer(tolower))
Reviewscorpus <- tm_map(Reviewscorpus, removeWords, stopwords("english"))
Reviewscorpus <- tm_map(Reviewscorpus, stripWhitespace)
Reviewscorpus <- tm_map(Reviewscorpus, stemDocument)


#clean Cropus for more data
dtm <- DocumentTermMatrix(Reviewscorpus)
dim(dtm) 
findFreqTerms(dtm, lowfreq=50)
summary(row_sums(dtm))
sort(col_sums(dtm), decreasing = TRUE)[1:10] 

dtmTfIdf <- weightTfIdf(dtm)
sort(col_sums(dtmTfIdf), decreasing = TRUE)[1:10]



tdm <- TermDocumentMatrix(Reviewscorpus)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(1235)
library(wordcloud)
library(wordcloud2)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)


w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)


library(syuzhet)
?iconv
input <- iconv(Reviewscorpus$text)
input[2]
sentiment <- get_nrc_sentiment(input)
head(sentiment, 20)
table(sentiment)


barplot(colSums(sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores on Reviews')


########################################################################
climate <- search_tweets("climate",n = 1000 , include_rts = FALSE , lang="en")
Gates_tweets <- get_timeline("@BillGates", n= 3200)
# Remove retweets
Gates_tweets_organic <- Gates_tweets[Gates_tweets$is_retweet==FALSE, ]
# Remove replies
Gates_tweets_organic <- subset(Gates_tweets_organic, is.na(Gates_tweets_organic$reply_to_status_id))


# Keeping only the retweets
Gates_retweets <- Gates_tweets[Gates_tweets$is_retweet==TRUE,]
# Keeping only the replies
Gates_replies <- subset(Gates_tweets, !is.na(Gates_tweets$reply_to_status_id))

