library(quanteda)
library(twitteR)
library(ROAuth)
library(httr)
library(base64enc)

api_key = "" # your api_key
api_secret = "" # your api_secret 
access_token = "" # your access_token 
access_token_secret = "" # your access_token_sceret 


#download.file(url = "http://curl.haxx.se/ca/cacert.pem",
#destfile = "cacert.pem")

setup_twitter_oauth('api_key',
                    'api_secret', # api secret
                    'access_token', # access token
                    'access_token_secret' # access token secret
)

tw = userTimeline("realDonaldTrump", n = 3000, includeRts = TRUE)
twDf = twListToDF(tw) ##convert to data frame
head(twDf)

twCorpus = corpus(twDf)
head(texts(twCorpus))
table(twCorpus[["isRetweet"]], useNA = "ifany")

#### obtain a clean DFM (document frequency matrix)
dfm2 = dfm(twCorpus,
           remove = c("amp", "rt", "https", "t.co", "will", "@realdonaldtrump", stopwords("english")),
           ngrams=1L,
           stem = F,
           remove_numbers = TRUE, 
           remove_punct = TRUE,
           remove_symbols = TRUE)

vdfm = dfm_trim(dfm2, min_count = 10, min_docfreq = 5)
## we want terms that appeared atleast 10 times

topfeatures(vdfm, n = 50)

library(topicmodels)

# we now export to a format that we can run the topic model with
dtm <- convert(vdfm, to="topicmodels")

# estimate LDA with K topics
K <- 20
lda <- LDA(dtm, k = K, method = "Gibbs", 
           control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))


lda

term <- terms(lda, 10) ##top 10 topics