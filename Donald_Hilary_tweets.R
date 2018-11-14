####################

setwd("F:\\DataMining_R\\3_LectureData\\section13\\tweets.csv")

library(tm)
library(lubridate)
library(dplyr)
library(plotly)
library(scales)

library(RSentiment)
library(stringr)
library(broom)
library(tidyr)
library(tidytext)


tweets=read.csv("tweets.csv")
head(tweets)

### explore tweeting frequency
temp = tweets %>% select(handle,is_retweet)
temp$is_retweet = ifelse(temp$is_retweet=='False','Original Tweets','Retweets')
temp= temp %>% group_by(is_retweet,handle) %>% summarise(n=n())

ggplot(temp, aes(x=handle, y=n, fill=is_retweet)) +
  geom_bar(position="dodge",stat='identity')

## how many tweets have candidates replied to?

temp = tweets %>% select(handle,in_reply_to_screen_name)
temp$in_reply_to_screen_name = ifelse(temp$in_reply_to_screen_name=='',NA,temp$in_reply_to_screen_name)
temp = na.omit(temp)
temp_1 = temp %>% group_by(handle) %>% summarise(n=n()) 

ggplot(temp_1, aes(x=handle, y=n)) +
  geom_bar(stat="identity", fill="lightblue", colour="black")+ggtitle("Number of Replies to Tweets by Candidate")

## top handles that got replied to

temp_1 = temp %>% group_by(handle,in_reply_to_screen_name) %>% summarise(n=n())
clinton = temp_1 %>% filter(handle=='HillaryClinton')
trump = temp_1 %>% filter(handle=='realDonaldTrump')
clinton

##How many retweets from each candidate.

temp =tweets %>% select(handle,is_retweet,original_author) %>% 
  filter(is_retweet=='True')%>% group_by(handle,original_author)%>% summarise(n=n())

temp %>% 
  group_by(handle) %>%
  summarise(s=sum(n))%>%
  ggplot(aes(x=handle,y=s))+
  geom_bar(fill='green',stat='identity')+
  xlab('Candidate')+
  ylab("Retweet Count")

## who were retweeted?
library(formattable)

clinton =filter(temp,handle=='HillaryClinton')
clinton = clinton %>% arrange(desc(n))
top_20 = clinton[1:20,2:3] #top 20 who were re-tweeted
top_20 %>%
  arrange(desc(n)) %>%
  formattable(list(n = color_bar("blue")), align = 'l')

trump =filter(temp,handle=='realDonaldTrump')
trump = trump %>% arrange(desc(n))
top_20 = trump[1:20,2:3]
top_20 %>%
  arrange(desc(n)) %>%
  formattable(list(n = color_bar("red")), align = 'l')