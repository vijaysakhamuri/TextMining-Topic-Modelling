#######################################################################
######## Explore multiple texts with tidytext

library(devtools)
devtools::install_github("bradleyboehmke/harrypotter")
## first 7 novels of harry potter

library(tidytext)
library(tidyverse)
library(stringr)
library(harrypotter)

philosophers_stone[1:2] #first 2 chapters of Philosopher's stone

##Tibbles are data frames
text_tb =tibble(chapter = seq_along(philosophers_stone),
                text = philosophers_stone)

head(text_tb)

text_tb=text_tb %>%
  unnest_tokens(word, text) #words in each chapter

head(text_tb)

### Multiple books

titles = c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
           "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
           "Deathly Hallows")

books = list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
             goblet_of_fire, order_of_the_phoenix, half_blood_prince,
             deathly_hallows)

series = tibble()

for(i in seq_along(titles)) {
  
  clean= tibble(chapter = seq_along(books[[i]]),
                text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series = rbind(series, clean)
}

# set factor to keep books in order of publication
series$book = factor(series$book, levels = rev(titles))

head(series)
tail(series)


### remove stop words and obtain common words

s=series %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

head(s)

## common words/book

bybook=series %>%
  anti_join(stop_words) %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10)

head(bybook)
tail(bybook)


## visualize word frequency/chapter

series %>%
  anti_join(stop_words) %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(book = factor(book, levels = titles),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")

# calculate percent of word use across all novels
potter_pct = series %>%
  anti_join(stop_words) %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

head(potter_pct)

frequency <- series %>%
  anti_join(stop_words) %>%
  count(book, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(potter_pct) %>%
  arrange(desc(book_words)) %>%
  ungroup()

frequency

##How correlated are the word frequencies between the entire series 
##and each book?
frequency %>%
  group_by(book) %>%
  summarize(correlation = cor(book_words, all_words),
            p_value = cor.test(book_words, all_words)$p.value)