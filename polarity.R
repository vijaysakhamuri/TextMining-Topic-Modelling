################################################################
### mugabe polarity

# content is negative, neutral or positive, returning "polarity" 
#values ranging from -1 for negative to +1 for positive

setwd("F:\\DataMining_R\\3_LectureData\\section12")
s=read.csv("mugabe1.csv")

library(dplyr)
library(qdap)

head(s)

x=qdap::polarity(s$text) 
x #overall negative

### explore more

# Split into retweets and original tweets
sp = split(s, s$isRetweet)
orig = sp[['FALSE']]
# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

pol = 
  lapply(orig$text, function(txt) {
    # strip sentence enders so each tweet is analyzed as a sentence,
    # and +'s which muck up regex
    gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
      # strip URLs
      gsub(' http[^[:blank:]]+', '', .) %>%
      # calculate polarity
      polarity()
  })

orig$emotionalValence = sapply(pol, function(x) x$all$polarity)

# As reality check, what are the most and least positive tweets
orig$text[which.max(orig$emotionalValence)]

orig$text[which.min(orig$emotionalValence)]

library(ggplot2)
ggplot(orig, aes(x = emotionalValence, y = retweetCount)) +
  geom_point(position = 'jitter') +
  geom_smooth()

polWordTables = 
  sapply(pol, function(p) {
    words = c(positiveWords = paste(p[[1]]$pos.words[[1]], collapse = ' '), 
              negativeWords = paste(p[[1]]$neg.words[[1]], collapse = ' '))
    gsub('-', '', words)  # Get rid of nothing found's "-"
  }) %>%
  apply(1, paste, collapse = ' ') %>% 
  stripWhitespace() %>% 
  strsplit(' ') %>%
  sapply(table)

library(tm)
## which are the positive and negative words

head(polWordTables)
