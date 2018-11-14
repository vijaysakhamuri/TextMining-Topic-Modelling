############## Model Binary response

### Predict movie sentiment (0 or 1) using text data as predictors

library(text2vec)
library(tm)

data("movie_review")
head(movie_review)
names(movie_review)
str(movie_review)

library(caret)
Train = createDataPartition(movie_review$sentiment, p=0.75, list=FALSE, times = 1)

# define training control
# train_control =trainControl(method="cv", number=10)

train = movie_review[Train, ] #75% training
test = movie_review[-Train, ]
##training data

### 1) vocabulary-based DTM. Here we collect unique terms 
### from all documents and mark each of them with a unique 
### ID using the create_vocabulary()

it = itoken(train$review, preprocessor = tolower,
            tokenizer = word_tokenizer,
            ids = train$id)
## iterator over tokens with the itoken() function.

v = create_vocabulary(it)

#  represent documents in vector space
vectorizer = vocab_vectorizer(v)

##testing data

it2 = itoken(test$review, preprocessor = tolower,
             tokenizer = word_tokenizer,
             ids = test$id)
# v2 = create_vocabulary(it2)

# pruned_vocab2 = prune_vocabulary(v2, term_count_min = 10, doc_proportion_max = 0.5, doc_proportion_min = 0.001)
# vectorizer2 = vocab_vectorizer(v2)


##document term matrix (vocab based)
dtm_train = create_dtm(it, vectorizer)
dtm_test = create_dtm(it2, vectorizer)


# get tf-idf matrix from bag-of-words matrix

##term frequency-inverse document frequency: quantify importance
## of a term in a document

## term-weighting scheme: normalize DTM, 
##also increase the weight of terms which are specific to a single document
##  decrease the weight for terms used in most documents

tfidf <- TfIdf$new()# define tfidf model

# fit the model to the train data and transform 
#it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)

dtm_test_tfidf <- fit_transform(dtm_test, tfidf)


################# Apply the GLMNET model

library(glmnet)

glmnet_classifier =cv.glmnet(x = dtm_train_tfidf, y = train[['sentiment']], 
                             family = 'binomial', 
                             # L1 penalty
                             alpha = 1,
                             # interested in the area under ROC curve
                             type.measure = "auc",
                             # 5-fold cross-validation
                             nfolds = 5,
                             # high value is less accurate, but has faster training
                             thresh = 1e-3,
                             # again lower number of iterations for faster training
                             maxit = 1e3)

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

## Predict on unseen data
preds <- predict(glmnet_classifier, dtm_test_tfidf, type= 'response')[, 1]
glmnet:::auc(as.numeric(test$sentiment), preds)
## how accurately can the sentiments be identified from text
