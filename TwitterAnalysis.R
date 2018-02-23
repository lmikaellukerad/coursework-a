# CS4125 Seminar Research Methodology for Data Science
# Coursework assignment A - Part 2, Question 1 - Twitter sentiment analysis
# 2017
#
# This code requires the following file: 
# sentiment3.R, negative-words.txt, and positive-words.txt.
#
#
# this is based on youtube https://youtu.be/adIvt_luO1o
# also see https://silviaplanella.wordpress.com/2014/12/31/sentiment-analysis-twitter-and-r/
############################################################################################

#setwd("~/surfdrive/Teaching/IN4125 - Seminar Research Methodology for Data Science/courseworkA") 
# apple , note use / instead of \, which used by windows


#install.packages("twitteR", dependencies = TRUE)
library(twitteR)
#install.packages("RCurl", dependencies = T)
library(RCurl)
#install.packages("bitops", dependencies = T)
library(bitops)
#install.packages("plyr", dependencies = T)
library(plyr)
#install.packages('stringr', dependencies = T)
library(stringr)
#install.packages("NLP", dependencies = T)
library(NLP)
#install.packages("tm", dependencies = T)
library(tm)
#install.packages("wordcloud", dependencies=T)
#install.packages("RColorBrewer", dependencies=TRUE)
library(RColorBrewer)
library(wordcloud)
#install.packages("reshape", dependencies=T)
library(reshape)

################### functions

  
clearTweets <- function(tweets, excl) {
  
  tweets.text <- sapply(tweets, function(t)t$getText()) #get text out of tweets 

  
  tweets.text = gsub('[[:cntrl:]]', '', tweets.text)
  tweets.text = gsub('\\d+', '', tweets.text)
  tweets.text <- str_replace_all(tweets.text,"[^[:graph:]]", " ") #remove graphic
  
  
  corpus <- Corpus(VectorSource(tweets.text))
  
  corpus_clean <- tm_map(corpus, removePunctuation)
  corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))
  corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
  corpus_clean <- tm_map(corpus_clean, removeNumbers)
  corpus_clean <- tm_map(corpus_clean, stripWhitespace)
  corpus_clean <- tm_map(corpus_clean, removeWords, c(excl,"http","https","httpst"))
  

  return(corpus_clean)
} 


## capture all the output to a file.

sink("output.txt")

################# Collect from Twitter

# for creating a twitter app (apps.twitter.com) see youtube https://youtu.be/lT4Kosc_ers
#consumer_key <-'your key'
#consumer_scret <- 'your secret'
#access_token <- 'your access token'
#access_scret <- 'your access scret'

source("your_twitter.R") #this file will set my personal variables for my twitter app, adjust the name of this file. use the provide template your_twitter.R

setup_twitter_oauth(consumer_key,consumer_scret, access_token,access_scret) #connect to  twitter app


##### This example uses the following 3 celebrities: Donald Trump, Hillary Clinton, and Bernie Sanders
##  You should replace this with your own celebrities, at least 3, but more preferred 
##  Note that it will take the computer some to collect the tweets

tweets_T <- searchTwitter("#KatyPerry", n=1000, lang="en", resultType="recent") #1000 recent tweets about Donald Trump, in English (I think that 1500 tweets is max)
tweets_C <- searchTwitter("#BarackObama", n=1000, lang="en", resultType="recent") #1000 recent tweets about Hillary Clinton
tweets_B <- searchTwitter("#EllenDeGeneres", n=1000, lang="en", resultType="recent") #1000 recent tweets about Bernie Sanders



######################## WordCloud
### This not requires in the assignment, but still fun to do 

# based on https://youtu.be/JoArGkOpeU0
corpus_T<-clearTweets(tweets_T, c("")) #remove also some campain slogans
wordcloud(corpus_T, max.words=50)

corpus_C<-clearTweets(tweets_C, c(""))
wordcloud(corpus_C,  max.words=50)

corpus_B<-clearTweets(tweets_B, c(""))#wordcloud(corpus_B,  max.words=50)
##############################






######################## Sentiment analysis

tweets_T.text <- laply(tweets_T, function(t)t$getText()) #get text out of tweets 
tweets_C.text <- laply(tweets_C, function(t)t$getText()) #get text out of tweets
tweets_B.text <- laply(tweets_B, function(t)t$getText()) #get text out of tweets



#taken from https://github.com/mjhea0/twitter-sentiment-analysis
pos <- scan('positive-words.txt', what = 'character', comment.char=';') #read the positive words
neg <- scan('negative-words.txt', what = 'character', comment.char=';') #read the negative words

source("sentiment3.R") #load algoritm
# see sentiment3.R form more information about sentiment analysis. It assigns a intereger score
# by substracitng the number of occurrence of negative words from that of positive words

analysis_T <- score.sentiment(tweets_T.text, pos, neg)
analysis_C <- score.sentiment(tweets_C.text, pos, neg)
analysis_B <- score.sentiment(tweets_B.text, pos, neg)


sem<-data.frame(analysis_T$score, analysis_C$score, analysis_B$score)


semFrame <-melt(sem, measured=c(analysis_T.score,analysis_C.score, analysis_B.score ))
names(semFrame) <- c("Candidate", "score")
semFrame$Candidate <-factor(semFrame$Candidate, labels=c("Katy Perry", "Barack Obama", "Ellen DeGeneres")) # change the labels for your celibrities

################## Below insert your own code to answer question 1. The data you need can be found in semFrame




######### stop redireting output.
sink()