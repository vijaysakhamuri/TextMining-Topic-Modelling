setwd("F:\\TextMining_R\\Data\\section7")

library(caret)
library(RTextTools)
spam=read.csv("spam.csv",stringsAsFactors = F)

head(spam)

### ham means: non-spam
### spam is spam

# remove empty columns
spam$X = NULL
spam$X.1 <- NULL
spam$X.2 <- NULL
names(spam) <- c("label","text")
levels(as.factor(spam$label))


head(spam)

##Split the dataset
set.seed(99)
Train = createDataPartition(spam$label, p=0.75, list=FALSE)

training = spam[ Train, ]
testing = spam[ -Train, ]

trace("create_matrix",edit=T) #debugging

dtMatrix <- create_matrix(training["text"]) #training predictors
## text data in training set
## creae DTM on training data


## Predictors:dtMatrix and response is label (ham or spam)
container <- create_container(dtMatrix, training$label, trainSize=1:nrow(training), virgin=FALSE)

##ML model
## implement linear SVM on container (which contains our Xs and Y)
model <- train_model(container, "SVM", kernel="linear", cost=1)

## how well does our model perform on unseen data?
## create DTM from test dataset
predMatrix <- create_matrix(testing["text"], originalMatrix = dtMatrix)


predSize = nrow(testing)
#container for 
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

## implemne the SVM model on testing text predictors
results <- classify_model(predictionContainer, model)

head(results)
