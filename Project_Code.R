#Get data

train <- read.csv("train.csv")
test = read.csv("test.csv")

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
