library(caret)
library(InformationValue)
library(ISLR)
library(ROSE)
#Get data
train <- read.csv("training.csv")
test = read.csv("testing.csv")
# Predictions for Fraud converted into Numerical data, 1 = Fraud, 0 = Not Fraud
train$FRAUD_NONFRAUD <- ifelse(train$FRAUD_NONFRAUD == "Fraud",1,0)
test$FRAUD_NONFRAUD <- ifelse(test$FRAUD_NONFRAUD == "Fraud",1,0)
#look at the data
head(train)


counts <- aggregate(train$FRAUD_NONFRAUD, by=list(train$CUST_STATE), FUN=length)
colnames(counts) <- c("State","Fraud")
barplot(counts$Fraud, names=counts$State)
#Take out only the numerical data columns and look for any significant relationship
new_train = train[1:5]
new_test = test[1:5]

new_train$FRAUD_NONFRAUD <- train$FRAUD_NONFRAUD
new_test$FRAUD_NONFRAUD <- test$FRAUD_NONFRAUD

pairs(new_train)
pairs(new_test)

head(new_train)
#To make absolutely sure, look at coorelation values before creating the model
cor(new_train)

attach(new_train)

set.seed(42069)

for(i in 2:5){
  y <- colnames(new_train)[i]
  y <- new_train[ , y]
  x <- new_train$FRAUD_NONFRAUD
  plot(x,y, name=y)
}

for(i in 2:5){
  print('-------------------------------------------------------------------------------------')
  predictor <- colnames(new_train)[i]
  print(predictor)
  model <- glm(FRAUD_NONFRAUD ~ new_train[ , predictor])
  #plot(model)
  #print(summary(
  #  model
  #))
  print(sum(resid(
    model
  ))^2)
  predicted <- predict(model, new_test, type="response")
  new_test$default <- ifelse(new_test$FRAUD_NONFRAUD=="Yes", 1, 0)
  optimal <- optimalCutoff(new_test$default, predicted)[1]
  #print(length(new_test$default))
  #print(length(predicted))
  print(confusionMatrix(new_test$default, predicted))
  #print(sensitivity(new_test$default, predicted))
  #print(specificity(new_test$default, predicted))
  #print(misClassError(new_test$default, predicted, threshold=optimal))
  
  print('-------------------------------------------------------------------------------------')
}
for(i in 2:5){
  print('-------------------------------------------------------------------------------------')
  predictor <- colnames(new_train)[i]
  print(predictor)
  model <- glm(FRAUD_NONFRAUD ~ new_train[ , predictor])
  #plot(model)
  #print(summary(
  #  model
  #))
  print(sum(resid(
    model
  ))^2)
  predicted <- predict(model, new_test, type="response")
  new_test$default <- ifelse(new_test$FRAUD_NONFRAUD=="Yes", 1, 0)
  optimal <- optimalCutoff(new_test$default, predicted)[1]
  #print(length(new_test$default))
  #print(length(predicted))
  print(confusionMatrix(new_test$default, predicted))
  #print(sensitivity(new_test$default, predicted))
  #print(specificity(new_test$default, predicted))
  #print(misClassError(new_test$default, predicted, threshold=optimal))
  
  print('-------------------------------------------------------------------------------------')
}


model <- glm(FRAUD_NONFRAUD ~ new_train$ACCT_PRE_TRAN_AVAIL_BAL:new_train$TRAN_AMT)




model <- glm(FRAUD_NONFRAUD ~ new_train$ACCT_PRE_TRAN_AVAIL_BAL:new_train$CUST_AGE)
predicted <- predict(model, new_test, type="response")
roc.curve(new_test$FRAUD_NONFRAUD, predicted, plotit=TRUE, add.roc=FALSE, n.threshold=100)

optimal <- optimalCutoff(test$default, predicted)[1]
print(confusionMatrix(new_test$default, predicted))
print(sensitivity(new_test$default, predicted))
print(specificity(new_test$default, predicted))
print(misClassError(new_test$default, predicted, threshold=optimal))

model <- glm(FRAUD_NONFRAUD ~ new_train$TRAN_AMT + new_train$CUST_AGE)
predicted <- predict(model, new_test, type="response")
optimal <- optimalCutoff(test$default, predicted)[1]
print(confusionMatrix(new_test$default, predicted))
print(sensitivity(new_test$default, predicted))
print(specificity(new_test$default, predicted))
print(misClassError(new_test$default, predicted, threshold=optimal))


print('transaction amount is important for predicting fraud')
print('p-value is not usefull because of how many observations there are in the dataset as an
      alternative we used a confusion matrix and RSS')
print('previous transaction balance was not as useful as some of the other features in the dataset.')
print('it was found that there are certain regions that have higher fraud than others. California has a higher totality of 
      fraud whereas Arizona and Texas has very high rates of raud comparative to its population and new york is low for both totality and per capita')
print('log transformation brought down effectiveness of transaction amt and is not useful for pre acct balance since there are negative numbers')
