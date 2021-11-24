#Get data

train <- read.csv("training.csv")
test = read.csv("testing.csv")

# Predictions for Fraud converted into Numerical data, 1 = Fraud, 0 = Not Fraud
train$FRAUD_NONFRAUD <- ifelse(train$FRAUD_NONFRAUD == "Fraud",1,0)

#look at the data
head(train)
head(test)

#Take out only the numerical data columns and look for any significant relationship
new_train = train[1:5]
new_train$FRAUD_NONFRAUD <- train$FRAUD_NONFRAUD
pairs(new_train)

#To make absolutely sure, look at coorelation values before creating the model
cor(new_train)

model1 = glm( as.factor(FRAUD_NONFRAUD) ~ TRAN_AMT + STATE_PRVNC_TXT, data = train, family = binomial )


#Scenario 3
set.seed(666)
smallData = sample(1:nrow(train), 20, replace=FALSE)
smallTrain = train[smallData,]

one_boot <- function(data) {
  index <- sample(1:nrow(data), nrow(data), replace=T)
  model <- glm(FRAUD_NONFRAUD ~ ACCT_PRE_TRAN_AVAIL_BAL + TRAN_AMT + CUST_AGE + DVC_TYPE_TXT, data = data[index,], family = "binomial")
  return(coef(summary(model))[2:5,2]) 
  #This returns a single bootstraped estimate of standard error
}
one_boot(smallTrain)

BS_estimate <- function(num.boots) {
  all_the_boots = c()
  for (i in 1:num.boots) {
    all_the_boots = rbind(all_the_boots, one_boot(smallTrain))
  }
  return(c(mean(all_the_boots[,1]),mean(all_the_boots[,2]),mean(all_the_boots[,3]),mean(all_the_boots[,4])))
}

BS_estimate(100)


