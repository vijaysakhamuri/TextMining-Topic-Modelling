####################################################################
##### Doc2Vec for Multiclass Classification

library(text2vec)
library(tm)
library(purrrlyr)

conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

x=read.csv("Reviews.csv") %>%
  dmap_at('Text', conv_fun)

d=x[1:4000,]#first 4000 rows
head(d)

vars=c("Id","Score","Text")
##score (1-5): response variable
## predictors: text

df= d[,colnames(d) %in% vars] 

head(df)

df=na.omit(df)

head(df)

library(caret)

set.seed(99)

Train = createDataPartition(df$Score, p=0.75, list=FALSE)

# define training control
train_control =trainControl(method="cv", number=10)

train = df[Train, ] #75% training
test = df[-Train, ]

prep_fun <- tolower
tok_fun <- word_tokenizer


it_train <- itoken(train$Text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   ids = train$Id,
                   progressbar = TRUE)


it_test <- itoken(test$Text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  ids = test$Id,
                  progressbar = TRUE)

vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
## vector space
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)

# define tf-idf model
tfidf <- TfIdf$new()
# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)

fit.svm.1 <- train(x = as.matrix(dtm_train_tfidf), y= as.factor(train$Score), 
                   method="svmLinear2", 
                   metric="Accuracy", 
                   trControl = train_control, 
                   scale = FALSE, verbose = TRUE)

fit.svm.1
## Predict on unseen data
pred <- predict(fit.svm.1, newdata = as.matrix(dtm_test_tfidf))

pred=as.integer(pred)

mat = confusionMatrix(pred, test$Score)
accuracy = mat$overall['Accuracy']
accuracy
kappa = mat$overall['Kappa']
kappa
