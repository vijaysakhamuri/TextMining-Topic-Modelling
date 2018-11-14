############## Topic Modelling-Latent Dirichlet Allocation
##automatic discovery of themes in a collection of documents.

library(topicmodels)
library(ggplot2)
library(tm)
library(readr)
library(wordcloud)
library(plyr)


require(SnowballC)

setwd("C:/Users/ViJaY PaVan/Documents/R")

d=read.csv("Catalan.csv")
head(d)

text <- as.character(d$text)


sample <- sample(text, (length(text)))
corpus <- Corpus(VectorSource(list(sample)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, removeWords, stopwords('spanish'))
corpus <- tm_map(corpus, removeWords, stopwords('catalan'))
corpus <- tm_map(corpus, stemDocument)

dtm_up1 <- DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
dtm_up1

ap_lda1 <- LDA(dtm_up1, k = 2, control = list(seed = 1234))
ap_lda1

##extract 2 topics from tweets

library(tidytext)

ap_topics <- tidy(ap_lda1, matrix = "beta")
#per-topic-per-word probabilities
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
#visualization lets us understand the two topics that were 
#extracted from the tweets


ap_documents <- tidy(ap_lda1, matrix = "gamma")
ap_documents
#LDA also models each document as a mixture of topics
#50% words from topic1