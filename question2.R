setwd("/Users/aytacozkan/works/text.mining/exam/Final-Exam-18-19/")

library(tm)
library(NLP)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(tidyverse)
library(tidytext)
library(ggplot2)


fname <- "spamDataTxt.csv"
headset <- read.csv(fname, header = TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

df = data.frame(headset)
#remove odd characters
df$text <- sapply(df,function(row) iconv(row, "latin1", "ASCII", sub=""))
# (1 for spams and 0 for non spams).
spam <- subset(df, cls == 1)
summary(spam)

# names(spam)

# 2.1 Create a corpus with the messages considered as spam.
corps = VCorpus(VectorSource(df))
spamcorps = VCorpus(VectorSource(spam))
meta(corps[[1]])

spamcorp <- tm_map(spamcorps,content_transformer(tolower))
spamcorp <- tm_map(spamcorp, removeWords,stopwords("english"))

allcorp <- tm_map(corps,content_transformer(tolower))
allcorp <- tm_map(allcorp, removeWords,stopwords("english"))

wordcloud(spamcorp, min.freq = 50, random.order = FALSE,
          colors=brewer.pal(8, "Dark2"))

wordcloud(allcorp, min.freq = 50, random.order = FALSE,
          colors=brewer.pal(8, "Dark2"))





