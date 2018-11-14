#############################################################
########### Visualize the tf-idf of multiple texts

library(gutenbergr)
library(dplyr)
library(tidytext)
library(tidyverse)

physics = gutenberg_download(c(37729, 14725, 13476, 5001), 
                             meta_fields = "author")

physics_words = physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

physics_words

### compute the Tf-IDF of multiple physicts
physics_words = physics_words %>%
  bind_tf_idf(word, author, n) 
plot_physics <- physics_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

physics_words

library(ggplot2)
library(ggstance)
library(ggthemes)
library(viridis)

ggplot(plot_physics[1:20,], aes(tf_idf, word, fill = author, alpha = tf_idf)) +
  geom_barh(stat = "identity") +
  labs(title = "Highest tf-idf words in Classic Physics Texts",
       y = NULL, x = "tf-idf") +
  theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1), guide = FALSE) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_viridis(end = 0.6, discrete=TRUE) +
  theme(legend.title=element_blank()) +
  theme(legend.justification=c(1,0), legend.position=c(1,0))