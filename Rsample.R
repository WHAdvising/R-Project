#multinomial logistic regression

setwd("C:/Users/whugh/Desktop/WORK/Dad's Project")



#load data from the CSV file
mydatax = read.csv("C:/Users/whugh/Desktop/WORK/Dad's Project/t70.csv", sep=",", header=TRUE, na.strings=c("")) 
mydatay = read.csv("C:/Users/whugh/Desktop/WORK/Dad's Project/t74.csv", sep=",", header=TRUE, na.strings=c("")) 


attach(mydatax)
attach(mydatay)



head(mydata)
inputx <- mydatax[,c("FICO", "PaymentAmt","IntRate")]
inputy <- mydatay[,c("StatusAtTimeT","FICO", "OrigBal","IntRate")]

head(input)
# check the structure of the data
str(input)
str(mydata)
#input$FICO <- as.numeric(input$FICO)


library(caTools)
split <- sample.split(mydata, SplitRatio = 0.8)
train <- subset(mydata, split =="TRUE")
test <- subset(mydata, split =="FALSE")
nrow(test)
nrow(train)


require(nnet)


#Column Names
#(OrigDate,	OrigBal,	IntRate,	FICO,	PaymentAmt,	Country,	SalesCenter,	DRI_DP_PCT,	CustType,	OrigBal_PPYPriorToOrigDate,	OrigBal_UPGPriorToOrigDate,	RatioTotalPmtToInc,	RatioTotalBalToNW)


numorigdate <- as.numeric(mydatax$OrigDate)
numorigdatey <- as.numeric(mydatay$OrigDate)

multinomModelx <- multinom(StatusAtTimeT ~ PaymentAmt + DRI_DP_PCT + RatioTotalPmtToInc + FICO + numorigdate, data=mydatax) # multinom Model

multinomModely <- multinom(StatusAtTimeT ~ PaymentAmt + DRI_DP_PCT + RatioTotalPmtToInc + FICO + numorigdatey, data=mydatay) # multinom Model


summary (multinomModelx) # model summary

summary (multinomModely) # model summary

exp(coef(multinomModelx)) # ratio of the probability of choosing one outcome category over the probability of choosing the baseline category


z <- summary(multinomModelx)$coefficients/summary(multinomModelx)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


head(pp <- fitted(multinomModelx))


predicted_scores <- predict (multinomModel, mydata, "probs") # predict on new data

head(predicted_scores)

sum(predicted_scores[1,])

sum(predicted_scores[5,])

predicted_class <- predict (multinomModelx, mydatax)

table(predicted_class, mydatax$StatusAtTimeT)

mean(as.character(predicted_class) != as.character(train$StatusAtTimeT))

mean(as.character(predicted_class) == as.character(train$StatusAtTimeT))


#create Confusion Matrix
cm <- table(predicted_class, LoanStatus)
print(cm)

# calculate the percentage of misclassifications by summing the diagonal values and dividing by the sum of all of the values in matrix
1-sum(diag(cm))/sum(cm)

# another way of calculating the percentage of misclassifications by taking the average of all of the TRUE values in the vector resulting from comparing predicted class values to the actual class in the test dataset. In other words, by 
mean(as.character(predicted_class) != as.character(LoanStatus))

# you can also do a tailed z test to determine th confidence level for dependent variable  When the p value is small for a given variable, the confidence level is high.  
z <- summary(multinomModel)$coefficients/summary(multinomModel)$standard.errors
P <- (1 - pnorm(abs(z), 0, 1)) * 2
P

# also check correlation between variables. Note that that all the  variables in the data set passed need to be numeric
numeric_input = mydatax[,c("FICO", "PaymentAmt","DRI_DP_PCT", "RatioTotalPmtToInc")]
numeric_input[,5] <- numorigdate
library(PerformanceAnalytics)
chart.Correlation(numeric_input, method="spearman", histogram=TRUE, pch=16)


# create a function to publissh to Azure ML

LoanEventScorer = function(FICO, OrigBal, IntRate)
{
  require(nnet)
  return(
    predict(
      multinomModel, 
      data.frame("FICO" = FICO, "OrigBal" = OrigBal, "IntRate" = IntRate),
      "probs"
    )
  )
}

LoanEventScorer(500, 56000, 0.07)

library(AzureML)

ws <- workspace(
  id = "ffb55396a56e422aa18a0fc6fd656573",
  auth = "W4nm/3EhQ2bkPKj9iWFHfrTVX8fyTI3o5fwoC19qHriRjnE1AUfIfyKRvoF1fj8s8xYockxNhK971S79lYn1nA=="
)

newService2 <- publishWebService(ws, LoanEventScorer, "LoanEventScorerOnline",
                                 list("FICO"="numeric","OrigBal" = "numeric", "IntRate" = "numeric"), 
                                 list("#NAME?" = "numeric",  "ACT"="numeric", "DEF" = "numeric", "PPY" = "numeric",
                                      "RES" = "numeric", "RES_UPG_" = "numeric", "UPG" = "numeric"))


newService2 <- publishWebService(ws, LoanEventScorer, "LoanEventScorerOnline",
                                 list("FICO"="numeric","OrigBal" = "numeric", "IntRate" = "numeric"), 
                                 list("Prob" = "numeric"))



#show source code for multinom function
multinom
