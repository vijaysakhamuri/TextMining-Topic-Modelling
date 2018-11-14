####################################################################
###########

setwd("F:\\DataMining_R\\3_LectureData\\section13")

reviews=read.csv("deceptive-opinion.csv")
head(reviews)

names(reviews)

unique(reviews$polarity)


reviews$text = as.character(reviews$text)

##convert word to numbers
reviews$polarity = as.integer(reviews$polarity=="positive")
reviews$deceptive = as.integer(reviews$deceptive=="truthful")
str(reviews)

table(reviews$deceptive,reviews$polarity)

library(tm)
corpus = Corpus(VectorSource(reviews$text))

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, removeWords, c("hotel", "hotels", "chicago"))

corpus = tm_map(corpus, stemDocument)

freq = DocumentTermMatrix(corpus)
freq

findAssocs(freq, terms="room", corlimit=0.25)

newcorpus = as.data.frame(as.matrix(freq))
colnames(newcorpus) = make.names(colnames(newcorpus))

head(newcorpus)

newcorpus$deceptive = reviews$deceptive

library(caret)

set.seed(99)
Train = createDataPartition(newcorpus$deceptive, p=0.75, list=FALSE)
#split data in 75%-25% ratio

training = newcorpus[ Train, ] #75% data for training 
testing = newcorpus[ -Train, ] #25% testing

library(caTools)
library(e1071)
library(randomForest)

rfdeceptive = randomForest(as.factor(deceptive)~., data=training)

pred1 = predict(rfdeceptive, testing, type="class")

table(pred1, testing$deceptive)

confusionMatrix(pred1, testing$deceptive)

varImpPlot(rfdeceptive, n.var=10)
#important words that affects the accuracy of the model.