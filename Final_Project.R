library(ROAuth)
library(tm)
library(syuzhet)
library(twitteR)
library('SnowballC')

#set up to authenticate
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)


APX <- searchTwitter("#APEX", n=100)
APX =  strip_retweets(APX)

FTN <- searchTwitter("#Fortnite", n=100)
FTN =  strip_retweets(FTN)

VLT <- searchTwitter("#Valorant", n=100)
VLT =  strip_retweets(VLT)

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

df.APX <- twListToDF(APX)
df.FTN <- twListToDF(FTN)
df.VLT <- twListToDF(VLT)

head(df.VLT)

APX.text = clean(df.APX$text)
VLT.text = clean(df.VLT$text)
FTN.text = clean(df.FTN$text)



sentiment.info = function(text){ 
  
  text.sentiment = get_nrc_sentiment((text))
  sentiment.score = data.frame(colSums(text.sentiment[,]))
  names(sentiment.score) = 'Score'
  sentiment.score = cbind("sentiment" = rownames(sentiment.score), sentiment.score)
  rownames(sentiment.score) = NULL
  
  return(sentiment.score)
}


sentiment.APX = sentiment.info(APX.text)
sentiment.APX
#Graph sentiment
library(ggplot2)
ggplot(data = sentiment.APX, aes(x=sentiment, y= Score))+geom_bar(aes(fill=sentiment), stat = 'identity' )+
  theme(legend.position = "none") +
  xlab("Sentiments")+ylab("scores") + ggtitle("Sentiments")


sentiment.VLT = sentiment.info(VLT.text)
sentiment.VLT
#Graph sentiment
ggplot(data = sentiment.VLT, aes(x=sentiment, y= Score))+geom_bar(aes(fill=sentiment), stat = 'identity' )+
  theme(legend.position = "none") +
  xlab("Sentiments")+ylab("scores") + ggtitle("Sentiments")


sentiment.FTN = sentiment.info(FTN.text)
sentiment.FTN
#Graph sentiment
ggplot(data = sentiment.FTN, aes(x=sentiment, y= Score))+geom_bar(aes(fill=sentiment), stat = 'identity' )+
  theme(legend.position = "none") +
  xlab("Sentiments")+ylab("scores") + ggtitle("Sentiments")

##########

APX.corpus = Corpus(VectorSource(APX.text))
VLT.corpus = Corpus(VectorSource(VLT.text))
FTN.corpus = Corpus(VectorSource(FTN.text))

#Word cloud
#install.packages('wordcloud')
library(wordcloud)
wordcloud(APX.corpus, min.freq = 10, colors = brewer.pal(8,"Dark2"),max.words = 1000)
wordcloud(VLT.corpus, min.freq = 10, colors = brewer.pal(8,"Dark2"),max.words = 1000)
wordcloud(FTN.corpus, min.freq = 10, colors = brewer.pal(8,"Dark2"),max.words = 1000)




corpus = Corpus(VectorSource(c(APX.text,FTN.text, VLT.text)))

y= as.factor(c( rep('APX', length(APX.text)) , rep('FTN', length(FTN.text)),rep('VLT', length(VLT.text))  ))



DTM <- DocumentTermMatrix(corpus)
inspect(DTM)
sparse_DTM <- removeSparseTerms(DTM, 0.97)
inspect(sparse_DTM)
tweets <- as.data.frame(as.matrix(sparse_DTM))
#tweets <- as.data.frame(as.matrix(tweets))
colnames(tweets) <- make.names(colnames(tweets))
dim(tweets)

#Tree classification
library(tree)

tweets = data.frame(y, tweets)
head(tweets)

train.index <- sample(c(1:dim(tweets)[1]), floor( 0.7 * dim(tweets)[1] ), replace = FALSE)

tweets.test = tweets[-train.index,]

tree.tweets = tree(y ~.-y,data = tweets, subset = train.index)
tree.pred = predict(tree.tweets,tweets.test, type = "class")

table(tree.pred,tweets.test$y)


plot(tree.tweets)
text(tree.tweets)

#Pruning
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
table(tree.pred,tweets.test$y)
##
