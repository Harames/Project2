library(ROAuth)
library(tm)
library(syuzhet)
library(twitteR)
library('SnowballC')

#set up to authenticate
consumer_key = "pXwwnWa9fQQ74uuHsm9GXTim7"
consumer_secret = "bMPJNzwU4zz2XxowAHxPHzdBvtYQkeC3cu5Nvp2AFRfUiJwniC" 
access_token = "427051006-iu9A48vwbFJJoWDFOiTcK4DrT78KS5SGd2kMQTZG"
access_secret = "zBBzUr9ZnoJ4UOkNwJF9bwqhTYzjU0GPoPZWCMVQwAUdy"
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)


clean = function(text){
  text = tolower(text)
  text = gsub("@\\w+", "", text)
  text = gsub("[[:punct:]]", "", text)#remove punctuation
  text = gsub("http\\w+","", text)#remove links
  text = gsub("[ |\t]{2,}", "", text)#remove tabs
  text = gsub("^ ", "", text)#remove blank spaces
  text = gsub(" $", "", text)#remove blank spaces
  text = removeWords(text  ,  stopwords("en") )#remove stop words
  text = stemDocument(text)#Stems the document
  
  return(text)
}

sentiment.info = function(text){ 
  
  text.sentiment = get_nrc_sentiment((text))
  sentiment.score = data.frame(colSums(text.sentiment[,]))
  names(sentiment.score) = 'Score'
  sentiment.score = cbind("sentiment" = rownames(sentiment.score), sentiment.score)
  rownames(sentiment.score) = NULL
  
  return(sentiment.score)
}

###########################################################
#DO NOT RUN THIS SECTION OF THE CODE, IT WILL TAKE LITERALLY FOREVER
APX <- searchTwitter("#APEX", n=20000)
APX =  strip_retweets(APX)

FTN <- searchTwitter("#Fortnite", n=20000)
FTN =  strip_retweets(FTN)

VLT <- searchTwitter("#Valorant", n=20000)
VLT =  strip_retweets(VLT)

for (i in 1:25) {
  x = (i*200)-199
  y = ((i+1)*200)-200
  nam <- paste("df.APX", i, sep = "")
  assign(nam, twListToDF(APXnew[x:y]))
  nam <- paste("df.FTN", i, sep = "")
  assign(nam, twListToDF(FTNnew[x:y]))
  nam <- paste("df.VLT", i, sep = "")
  assign(nam, twListToDF(VLTnew[x:y]))
}
for (i in 1:25) {
  nam <- paste("APX.text", i, sep = "")
  nam2 <- paste("df.APX", i, sep = "")
  assign(nam, clean(get(nam2)$text))
  nam <- paste("FTN.text", i, sep = "")
  nam2 <- paste("df.FTN", i, sep = "")
  assign(nam, clean(get(nam2)$text))
  nam <- paste("VLT.text", i, sep = "")
  nam2 <- paste("df.VLT", i, sep = "")
  assign(nam, clean(get(nam2)$text))
}

for (i in 1:25) {
  nam <- paste("sentiment.APX", i, sep="")
  nam2 <- paste("APX.text", i, sep="")
  assign(nam, sentiment.info(get(nam2)))
  nam <- paste("sentiment.FTN", i, sep="")
  nam2 <- paste("FTN.text", i, sep="")
  assign(nam, sentiment.info(get(nam2)))
  nam <- paste("sentiment.VLT", i, sep="")
  nam2 <- paste("VLT.text", i, sep="")
  assign(nam, sentiment.info(get(nam2)))
}

bruh = data.frame()
for (i in 1:25) {
  nam <- paste("sentiment.APX", i, sep="")
  nam2 <- paste("sentiment.VLT", i, sep="")
  nam3 <- paste("sentiment.FTN", i, sep="")
  headers = c('Games', get(nam)['sentiment'][1,],get(nam)['sentiment'][2,],get(nam)['sentiment'][3,],get(nam)['sentiment'][4,],get(nam)['sentiment'][5,],get(nam)['sentiment'][6,],get(nam)['sentiment'][7,],get(nam)['sentiment'][8,],get(nam)['sentiment'][9,],get(nam)['sentiment'][10,])
  apxList = c('APX', get(nam)['Score'][1,],get(nam)['Score'][2,],get(nam)['Score'][3,],get(nam)['Score'][4,],get(nam)['Score'][5,],get(nam)['Score'][6,],get(nam)['Score'][7,],get(nam)['Score'][8,],get(nam)['Score'][9,],get(nam)['Score'][10,])
  vltList = c('VLT', get(nam2)['Score'][1,],get(nam2)['Score'][2,],get(nam2)['Score'][3,],get(nam2)['Score'][4,],get(nam2)['Score'][5,],get(nam2)['Score'][6,],get(nam2)['Score'][7,],get(nam2)['Score'][8,],get(nam2)['Score'][9,],get(nam2)['Score'][10,])
  ftnList = c('FTN', get(nam3)['Score'][1,],get(nam3)['Score'][2,],get(nam3)['Score'][3,],get(nam3)['Score'][4,],get(nam3)['Score'][5,],get(nam3)['Score'][6,],get(nam3)['Score'][7,],get(nam3)['Score'][8,],get(nam3)['Score'][9,],get(nam3)['Score'][10,])

  vgData <- data.frame(apxList,vltList,ftnList)
  vgData <- as.data.frame(t(vgData))
  colnames(vgData) <- headers
  
  bruh <- rbind(bruh, vgData)
  
}


######################################
library(tree)
bruh$Games = as.factor(bruh$Games)

train.index <- sample(c(1:dim(bruh)[1]), floor( 0.7 * dim(bruh)[1] ), replace = FALSE)
tweets.test = bruh[-train.index,]

tree.tweets = tree(Games ~.-Games,data = bruh, subset = train.index)
tree.pred = predict(tree.tweets,tweets.test, type = "class")

table(tree.pred,bruh[-train.index,]$Games)


plot(tree.tweets)
text(tree.tweets)
cv.tweets = cv.tree(tree.tweets, FUN = prune.misclass)
cv.tweets
#Size is the number of terminal nodes of each tree considered
#Dev is the cross validation error rate 

plot(cv.tweets$size, cv.tweets$dev,type = "b")
#plot(cv.tweets$k, cv.tweets$dev,type = "b")

#Prune the tree
prune.tweets = prune.misclass(tree.tweets,best = 3) #Choose the size that corresponds to the lowest dev
plot(prune.tweets)
text(prune.tweets)

tree.pred = predict(prune.tweets,tweets.test, type = 'class')
table(tree.pred,tweets.test$Game)
##
