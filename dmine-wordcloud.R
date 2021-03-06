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
library(httr) 
library(base64enc)
#--twitter authentication
options(httr_oauth_cache=T)
api_key <- "fEudPDoHEj9xz6gYKZ7Qc2f8V"
api_secret <- "P1WHRbwi8v9SV41a1BeMnmugmGjiJSjQQYGb6PhscrZJMQspMq"
access_token <- "11766682-euvpiRXoStHgcwyFSwVDnIncmhxZdr0wbF7PFoC9O"
access_token_secret <- "b3pzUvIijfYvBEAftFSac3rLLuo82Gy6BTo2KSwc6Jc7M"

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
  stopwords = c("climate", "change", "http", "https", stopwords("english")),
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

layout(matrix(c(2, 1), ncol=2), heights=c(4,1))
par(mar=c(0,0,0,8))
#text(x=0.5, y=0.5, "Title of my first plot")
cloudname <- "climate.png"
cloudpng <- png(paste0(string, "_", cloudname),width=8,height=8,units="in", res=300)
#mtext(format(Sys.time(), "%a %b %d %X %Y"), side = 3, col="blue", cex=1.5)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), main = "Title")
mtext(format(Sys.time(), "%a %b %d %X %Y"), side = 3, col="blue", cex=1.5)
#mtext(format(Sys.time(), "%a %b %d %X %Y"), side = 3, col="blue", cex=1.5)

#--set the wordcloud png name, which will be appended to the time stamp later
dev.off()
#system(paste("cp ", string, "_", cloudname, " ", cloudname, sep=""))
