#################################################################
####### Geo-locations of the Tweets

library(twitteR)
library(ROAuth)
library(httr)
library(base64enc)

setup_twitter_oauth('bqnkZc0Tah16HuyAXFUq5W7zf',
                    '5Hyyyv4AY0s2vNLwCZ24ZS6cNhCUvUIkANVLJcoslJyoKRZ3SH', # api secret
                    '3156899792-tS6Nj3XLD3m2olpC2XjwgM9Olrb9jT1FSjZ086t', # access token
                    'FBekm8O2aS5dw15yMIc14no1zEn6amAUBHWMxRJo3NyHH' # access token secret
)

search.string <- "#NBK"
no.of.tweets <- 2000

JTweets <- searchTwitter(search.string, n=no.of.tweets,lang="en",)

df <- do.call("rbind", lapply(JTweets, as.data.frame))
View(df)

userInfo <- lookupUsers(df$screenName)  
# Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF
head(userFrame)

locatedUsers <- !is.na(userFrame$location)  
# Keep only users with location info

head(locatedUsers)

library(dismo)
library(maps)
locations <- geocode(userFrame$location[locatedUsers])  
# Use amazing API to guess
head(locations)

# approximate lat/lon from textual location data.
with(locations, plot(longitude, latitude))

write.csv(locations,"JeruLocation.csv")

table(locations$interpretedPlace)

head(locations$interpretedPlace)