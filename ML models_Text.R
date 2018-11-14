#####################################################################

library(RTextTools)
## ML Library for text classification

data(USCongress)

head(USCongress)

str(USCongress)

## create dtm

doc_matrix = create_matrix(USCongress$text, language="english", removeNumbers=TRUE,
                           stemWords=TRUE, removeSparseTerms=.998)

#response ariable- major 
container = create_container(doc_matrix, USCongress$major, trainSize=1:4000,
                             testSize=4001:4449, virgin=FALSE)

### apply ML models on text data

models =train_models(container, algorithms=c("MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"))

###### obtain classified data

results=classify_models(container, models)

# VIEW THE RESULTS BY CREATING ANALYTICS
analytics <- create_analytics(container, results)

analytics