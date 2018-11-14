########## Examine multi-document corpus of texts


require(quanteda)

data(data_char_ukimmig2010)
head(data_char_ukimmig2010)

myCorpus = corpus(data_char_ukimmig2010)  # build a new corpus from the texts
summary(myCorpus)
##defined as a collection of texts that includes 
##document-level variables specific to each text

################################
######### add some document-level variables
#### docvars

docvars(myCorpus, "Party") = names(data_char_ukimmig2010)
docvars(myCorpus, "Year") = 2010
summary(myCorpus)

###add metadeta info

metadoc(myCorpus, "language") = "english"
metadoc(myCorpus, "docsource")  = paste("data_char_ukimmig2010", 1:ndoc(myCorpus), sep = "_")
summary(myCorpus, showmeta = TRUE)


texts(myCorpus)[3] #procure text from corpus

# make a dfm
# document frequency matrix
myDfm = dfm(myCorpus)
myDfm[, 1:5]

# make a dfm, removing stopwords and applying stemming
myStemMat = dfm(myCorpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
myStemMat[, 1:5]

topfeatures(myStemMat, 20) ##top 20 words