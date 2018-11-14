##### TF-IDF: term-weighting scheme

####### Tf-idf stands for term frequency-inverse document frequency,
######numerical statistic that is intended to reflect how 
##important a word is 

####  tf-idf value increases wrt  to the number of times a word 
#### appears in the document,
###but is often offset by the frequency of the word in the corpus
### helps to adjust for the fact that some words appear more frequently.

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words = austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

head(book_words)

## Total word count/book
total_words = book_words %>% 
  group_by(book) %>% summarize(total = sum(n))

head(total_words)


book_words = left_join(book_words, total_words)
book_words

######### Calculate the rank of words such that most frequent words have 
### lower ranks

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
##idf and thus tf-idf are zero for these extremely common words. 

head(book_words)

### Highest TF-IDF values (most important)
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf)) 

##visualization for these high tf-idf words 
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()
