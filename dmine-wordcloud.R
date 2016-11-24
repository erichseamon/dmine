#
# DATE:         December 2015
#
# STAGE:        dmine-wordcloud.R
#
# COMMENTS:     this accesses the twitter api using the twitteR package,
#               and finds the last 500 tweets using a particular phrase,
#               and then creates a word cloud of the most frequent words
#               around these tweets.  Uses the tm text mining package as well.
#
#
#--Setting the working directory an d clearing the workspace-----------
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
#--twitter authentication
options(httr_oauth_cache=T)
api_key <- "aoMASn5GMUb7cFciJiyHNn70z"
api_secret <- "zo6sOFA9x7waw1VGvi5R0i3gGn9hrmDDKcz7hH6mY6ehyjfuP3"
access_token <- "11766682-JXMveG0dc97FUpkF1o8EapLsGGMdyddbFKQpgKefH"
access_token_secret <- "bjy8lXYh5yroi7QzTkSw1in9k70ANTueRiLNUkZrvnz5Q"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
#-uses the twitter api and twitteR to search the last n number of tweets
mach_tweets = searchTwitter("climate+change", n=500, lang="en", resultType = "recent")
#mach_tweets = searchTwitter("climate+change", n=500, lang="en", geocode='46.732,-117.001, 100mi', resultType = "recent")
#--create a matrix using apply of the tweet texts
mach_text = sapply(mach_tweets, function(x) x$getText())
mach_text <- iconv(mach_text,to="utf-8")
# create a corpus
mach_corpus = Corpus(VectorSource(mach_text))
# create document term matrix applying some transformations
control <- list(
  removePunctuation = TRUE,
  stopwords = c("climate", "change", stopwords("english")),
  removeNumbers = TRUE,
  tolower = TRUE)

tdm <- TermDocumentMatrix(mach_corpus, control = control)
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)


setwd("/dmine/data/wordclouds")
#"%Y-%j-%H%M%S"

string <- format(Sys.time(), format = "%Y-%m-%d_%H:%M:%S")
cloudname <- "MachineLearningCloud.png"
cloudpng <- png(paste0(string, "_", cloudname), width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
#--set the wordcloud png name, which will be appended to the time stamp later
system(paste("cp ", string, "_", cloudname, " ", cloudname, sep=""))
dev.off()
