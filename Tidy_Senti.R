##############################################################
###### Tidy Sentiments

library(janeaustenr)
library(dplyr)
library(tm)
library(tidytext)
library(tidyverse)
library(qdapTools)
library(ggplot2)

austen_books_df=as.data.frame(austen_books(),stringsAsFactors=F)

head(austen_books_df)
summary(austen_books_df)

## isolate a book
emma=austen_books_df %>% group_by(book) %>% 
  filter(book == "Emma")

head(emma)
tail(emma)

#########cleaning

corpus <- Corpus(VectorSource(emma$text))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)

myDtm <- TermDocumentMatrix(corpus) #create a DTM

##require(tidytext)
terms=Terms(myDtm) 
head(terms)

ap_td = tidy(myDtm) #convert DTM in a "tidy" form
ap_td

## sentiment analysis using tidy text
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

tail(ap_sentiments)

## which words contribute to positivity
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 100) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip() #horizontal barplot