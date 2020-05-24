
# we can include addr_state in the model since it doesnt have any missing values

setwd("E:/r direct/Capstone Project/Project")

post_imputation_data<-read.csv("Post Imputation.csv",header=TRUE)
str(post_imputation_data)

# drop id , member_id, total_pymnt_inv , Column.0
which( colnames(post_imputation_data)=="total_pymnt_inv" )
which( colnames(post_imputation_data)=="member_id" )
which( colnames(post_imputation_data)=="id" )

final_loan_data<-post_imputation_data[, -c(1,2,3,27)]
str(final_loan_data)

# final_loan_data$loan_status=as.factor(final_loan_data$loan_status)
# str(final_loan_data)


#---------------- data split into train (70%) and test (30%) --------------------

library(caret)

# Setting the seed to 12345 for the purpose of reproducible results
set.seed(12345)

inTrain <- createDataPartition(final_loan_data$loan_status,p=0.70,list = FALSE)
Training <- final_loan_data[inTrain,]
Testing <- final_loan_data[-inTrain,]

c(nrow(Training), nrow(Testing))

prop.table(table(Training$loan_status))

prop.table(table(Testing$loan_status))


table(Testing$loan_status)

# ------------ multinomial LR --------------

library(foreign)
library(nnet)
library(stargazer)
library(MASS)

table(Training$loan_status)

# Training$loan_status1 = relevel(Training$loan_status, ref = "1")
# str(Training)
# which( colnames(Training)=="loan_status" )
# Training1<-Training[,-c(10)]
# str(Training1)
# multi1 = multinom(loan_status1 ~ ., data=Training1)
# m1 <- polr(loan_status ~ ., data=Training, Hess=TRUE)

#---------------

# BUILD THE MULTINOMIAL LOGISTIC REGRESSION MODEL:

multi1 = multinom(loan_status ~ ., data=Training)

#--------------------


predicted_scores <- predict (multi1, Testing, "probs")
predicted_class <- predict (multi1, Testing)
table(predicted_class, Testing$loan_status)
mean((predicted_class) != (Testing$loan_status))


# OVERALL SIGNIFICANCE/VALIDITY OF THE MODEL (log likelihood test):
library(lmtest)
lrtest(multi1)

# p-value : < 2.2e-16 ***
# Interpretation: The overall test of the model significance based on the chisq test above 
# is overwhelmingly significant (since p<0.05) indicating the likelihood of the response variable
# depends upon the independent variables provided. It implies that 
# the null hypothesis of "all the coefficient estimates are Zero" is rejected 
# and we conclude that at least one coefficient estimate is non-Zero.

# -------------------------

#	MEASURE OF FIT: CALCULATE MCFadden R-Square

library(pscl)
# psuedo R-sq command
pR2(multi1)
  
# Mc-fadden R-sq : 5.291921e-01 - 52.9% goodness of fit is EXCELLENT and it indicates very good predictive ability
# It also indicates- 52.9% of the uncertainty of the intercept only model has been explained by the model we have built. 

#----------------------------
# 1. summary - interpret the coefficients
summary(multi1)
# multi1$fitted.values

# 2. conclusions on significance of the independent variables
# The multinom() function does not provide p-values, 
# we can get significance of the coefficients using the stargazer() function from the package -stargazer.
library(stargazer)
stargazer(multi1, type="text")


# 3. explanatory power of odds and interpretation
odds <- exp(coef(multi1))
odds
probablity <- odds/(odds+1)
probablity


# ---------------- Prediction on Test data ---------------

# Testing[, c("predicted_status")] <- predict(multi1, newdata=Testing, type="probs")
Testing[, c("predicted_status")] <- predict(multi1, newdata=Testing, type="class")
str(Testing)

which( colnames(Testing)=="loan_status" )
which( colnames(Testing)=="predicted_status" )
Testing1<- Testing[,c(10,36)]
head(Testing1)
str(Testing1)

# View(Testing1)
# Testing1$loan_status <- as.numeric(Testing1$loan_status)
Testing1$predicted_status <- as.integer(Testing1$predicted_status)

Testing1$Match <- NULL
Testing1[Testing1$loan_status > Testing1$predicted_status,"Match"] <- 0
Testing1[Testing1$loan_status < Testing1$predicted_status,"Match"] <- 0
Testing1[Testing1$loan_status == Testing1$predicted_status,"Match"] <- 1
table(Testing1$Match)

255338/(255338+10875)

# 1- misClassError(Testing1$loan_status,Testing1$predicted_status)
# plotROC(Testing1$loan_status,Testing1$predicted_status)
# confusionMatrix(Testing1$loan_status,Testing1$predicted_status)
# Concordance(Testing1$loan_status,Testing1$predicted_status)


#--------------------------------------------

# Running the ordered logit model
# if(missing(start)) { 
#   # try something that should always work -tjb 
#   u <- as.integer(table(y)) 
#   u <- (cumsum(u)/sum(u))[1:q] 
#   zetas <- 
#     switch(method, 
#            "logistic"= qlogis(u), 
#            "probit"=   qnorm(u), 
#            "cauchit"=  qcauchy(u), 
#            "cloglog"=  -log(-log(u)) ) 
#   s0 <- c(rep(0,pc),zetas[1],log(diff(zetas))) 
#   
# }
# source('fixed-polr.R') 


# m1 <- polr(loan_status ~ ., data=final_loan_data, Hess=TRUE)
# summary(m1)
# 
# # Getting coefficients and p-values
# m1.coef <- data.frame(coef(summary(m1)))
# m1.coef$pval = round((pnorm(abs(m1.coef$t.value), lower.tail = FALSE) * 2),2)
# m1.coef
# 
# library(stargazer)
# stargazer(m1, type="html", out="m1.htm")
# 
# m1.or=exp(coef(m1))
# m1.or
# 
# library(stargazer)
# stargazer(m1, type="html", coef=list(m1.or), p.auto=FALSE, out="m1or.htm")
# 
# m1.pred <- predict(m1, type="probs")
# summary(m1.pred)
# 
# 
# setup1 <- data.frame(x1=rep(mean(mydata$x1),2),
#                      x2=rep(mean(mydata$x2),2),
#                      x3=c(1,2))
# setup1
# 
# setup1[, c("pred.prob")] <- predict(m1, newdata=setup1, type="probs")
# setup1
# 
# setup1[, c("pred.prob")] <- predict(m1, newdata=setup1, type="class")
# setup1
# 
# library(erer)
# x <- ocME(m1, x.mean=TRUE)
# x
# x$out

#----------------------------------------------------

