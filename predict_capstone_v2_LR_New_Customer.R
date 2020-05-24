
setwd("E:/r direct/Capstone Project/Project")

post_imputation_data<-read.csv("Post Imputation final.csv",header=TRUE)


table(post_imputation_data$loan_status)

str(post_imputation_data)
View(post_imputation_data)

# dropping the columns which are not needed for the model
# drop id , member_id, total_pymnt_inv , Column.0
which( colnames(post_imputation_data)=="X" )
##which( colnames(post_imputation_data)=="member_id" )
##which( colnames(post_imputation_data)=="id" )

final_loan_data<-post_imputation_data[, -c(1)]
str(final_loan_data)

colnames(final_loan_data)

final_loan_data<-final_loan_data[, c(1,2,5,6,7,9,10,12,15,34,35)]

write.csv(final_loan_data,file="New Customer.csv")

View(final_loan_data)

#---------------- data split into train (70%) and test (30%) --------------------

library(caret)

# Setting the seed to 12345 for the purpose of reproducible results
set.seed(786449)

inTrain <- createDataPartition(final_loan_data$loan_status,p=0.70,list = FALSE)
Training <- final_loan_data[inTrain,]
Testing <- final_loan_data[-inTrain,]

c(nrow(Training), nrow(Testing))

prop.table(table(Training$loan_status))

prop.table(table(Testing$loan_status))


# ------------ multinomial Logistic Regression --------------

library(foreign)
library(nnet)
library(stargazer)
library(MASS)

table(Training$loan_status)

# BUILD THE MULTINOMIAL LOGISTIC REGRESSION MODEL:

multi1 = multinom(loan_status ~ ., data=Training)


# OVERALL SIGNIFICANCE/VALIDITY OF THE MODEL (log likelihood test):
library(lmtest)
lrtest(multi1)


#	MEASURE OF FIT: CALCULATE MCFadden psuedo R-Square
library(pscl)
pR2(multi1)


# 1. summary - interpret the coefficients
summary(multi1)


# 2. conclusions on significance of the independent variables
library(stargazer)
stargazer(multi1, type="text")

# 3. explanatory power of odds and interpretation
odds <- exp(coef(multi1))
odds
probablity <- odds/(odds+1)
probablity


#----------------- Prediction on Train data--------------

# 
Training_p <- Training
Training_p[, c("predicted_probability")] <- predict(multi1, newdata=Training_p, type="probs")

Training1 <- Training
Training1[, c("predicted_status")] <- predict(multi1, newdata=Training1, type="class")


# CONFUSION MATRIX FOR TRAIN DATA

confusion_matrix_train_data <-table(Training1$predicted_status,Training1$loan_status)
confusion_matrix_train_data
t<-confusion_matrix_train_data

# ACCURACY FOR TRAIN DATA

Accuracy_train_data <- sum(diag(t))/sum(t)
Accuracy_train_data

# Recall = TP / (TP+FN)
# Precision = TP / (TP+FP)

rowsums = apply(t, 1, sum)
colsums = apply(t, 2, sum)
diag=diag(t)

precision = diag / colsums 
recall = diag / rowsums 

f1 = 2 * precision * recall / (precision + recall) 

# Train data : precision, recall and F1 score for all the classes (that is 1,2 and 3)
data.frame(precision, recall, f1)


# ---------------- Prediction on Test data --------------

Testing_p <- Testing
Testing_p[, c("predicted_probability")] <- predict(multi1, newdata=Testing_p, type="probs")

Testing1 <- Testing
Testing1[, c("predicted_status")] <- predict(multi1, newdata=Testing1, type="class")

# CONFUSION MATRIX FOR TEST DATA

confusion_matrix_test_data <-table(Testing1$predicted_status,Testing1$loan_status)
confusion_matrix_test_data
t1<-confusion_matrix_test_data

# accuracy
Accuracy_test_data <- sum(diag(t1))/sum(t1)
Accuracy_test_data


rowsums = apply(t1, 1, sum)
colsums = apply(t1, 2, sum)
diag=diag(t1)

precision = diag / colsums 
recall = diag / rowsums 

f1 = 2 * precision * recall / (precision + recall) 

# Test data : precision, recall and F1 score for all the classes (that is 1,2 and 3)
data.frame(precision, recall, f1)



