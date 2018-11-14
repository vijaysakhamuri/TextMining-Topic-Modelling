#############
##perform topic modeling to see whether the algorithm 
##can correctly distinguish the four groups/themes of 4 books

library(gutenbergr)
library(tidytext)
library(stringr)
library(topicmodels)
library(dplyr)
library(tidyr)

titles = c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
           "Pride and Prejudice", "Wuthering Heights")


books = gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")



##different chapters
by_chapter = books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

#unnest_tokens() to separate them into words, 
#then remove stop_words
# split into words
by_chapter_word = by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts = by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

#Create a Document Term Matrix(dtm)
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

## k=4 as we have 4 books thus 4 distinct themes
chapters_lda = LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

#examine per-topic-per-word probabilities.
chapter_topics =tidy(chapters_lda, matrix = "beta")
chapter_topics

##top 10 terms for each topic
top_terms = chapter_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

library(ggplot2)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

##we may want to know which topics are associated with each document
chapters_gamma = tidy(chapters_lda, matrix = "gamma")
chapters_gamma

chapters_gamma = chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)
## WH, War of the Worlds, and 
##Twenty Thousand Leagues Under the Sea were uniquely identified as a single topic each
