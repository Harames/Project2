set.seed(8675309)
library('MASS')
test.index = sample(c(1:dim(train)[1]),.2*dim(train)[1], replace = FALSE  )

lda.fit = lda(FRAUD_NONFRAUD ~ ACCT_PRE_TRAN_AVAIL_BAL + TRAN_AMT + CUST_AGE + DVC_TYPE_TXT, data = train, family = binomial, subset = -test.index)
lda.fit

lda.pred = predict(lda.fit, train[test.index,])
lda.class = lda.pred$class
table(lda.class,train[test.index,]$FRAUD_NONFRAUD)

# model1 = glm(FRAUD_NONFRAUD ~ TRAN_AMT + STATE_PRVNC_TXT, data = train, family = binomial )

qdamodel = qda(FRAUD_NONFRAUD ~ ACCT_PRE_TRAN_AVAIL_BAL + TRAN_AMT + CUST_AGE + DVC_TYPE_TXT,data = train, subset = -test.index,
               family=binomial)
qdamodel

qda.pred = predict(qdamodel, train[test.index,])
qda.class = qda.pred$class
table(qda.class,train[test.index,]$FRAUD_NONFRAUD)

print("QDA IS BETTER THAN LDA BY TABLE DATA, AND KNN CANT DO THAT MANY VARS")
print("WE KEPT THE SAME INPUT VARIABLES, THEY ARE ALL SIGNIFICANT AND I LIKE CONSISTENCY")

qdamodel2 = qda(FRAUD_NONFRAUD ~ ACCT_PRE_TRAN_AVAIL_BAL + CUST_AGE + DVC_TYPE_TXT,data = train, subset = -test.index,
               family=binomial)
qdamodel2

qda.pred = predict(qdamodel2, train[test.index,])
qda.class = qda.pred$class
table(qda.class,train[test.index,]$FRAUD_NONFRAUD)

print("TRAN_AMT IS IMPORTANT, THE ACCURACY OF THE MODEL GOT WORSE WHEN YOU REMOVED IT.")


#Feel free to change this, I just threw up words lol
print("BECASE THEY ARE MORE INTERESTED IN WHAT THE BEST RESULTS CAN BE LOOKING INTO THE FUTURE. IF YOU TELL THEM THAT FOCUSING ON THESE LEADS TO BETTER RESULTS, THAT'S ALL THEY NEED TO KNOW, AND THEY WON'T CARE AS MUCH HOW YOU GOT THERE. (just me spitballing, this can and probs should be changed.)")
