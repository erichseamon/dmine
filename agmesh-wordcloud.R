library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

#--twitter authentication
options(httr_oauth_cache=T)

api_key <- "aoMASn5GMUb7cFciJiyHNn70z"
api_secret <- "zo6sOFA9x7waw1VGvi5R0i3gGn9hrmDDKcz7hH6mY6ehyjfuP3"
access_token <- "11766682-JXMveG0dc97FUpkF1o8EapLsGGMdyddbFKQpgKefH"
access_token_secret <- "bjy8lXYh5yroi7QzTkSw1in9k70ANTueRiLNUkZrvnz5Q"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

mach_tweets = searchTwitter("machine learning", n=500, lang="en")

mach_text = sapply(mach_tweets, function(x) x$getText())

# create a corpus
mach_corpus = Corpus(VectorSource(mach_text))

# create document term matrix applying some transformations

control <- list(
      removePunctuation = TRUE,
      stopwords = c("machine", "learning",stopwords("english")),
      removeNumbers = TRUE,
      tolower = TRUE)

tdm <- TermDocumentMatrix(mach_corpus, control = control)
                         


# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
#setwd("/git/dmine/")
png("MachineLearning2Cloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
