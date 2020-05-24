
setwd("E:/r direct/Capstone Project/Project")

final_loan_data<-read.csv("New Customer Two Level.csv",header=TRUE)
str(final_loan_data)
colnames(final_loan_data)
final_loan_data<-final_loan_data[, -c(1,2)]
View(final_loan_data)
str(final_loan_data)

colnames(final_loan_data)

final_loan_data<-final_loan_data[, c(1,2,5,6,7,8,10,11,12,15,18)]

 library(ROSE)
 set.seed(7886865)
 final_loan_data<- ovun.sample(loan_status ~ ., data = final_loan_data, method = "over", p = 0.5)$data
 prop.table(table(final_loan_data$loan_status))
 nrow(final_loan_data)


final_loan_data$loan_status = as.factor(final_loan_data$loan_status)
str(final_loan_data)

#---------------- data split into train (70%) and test (30%) --------------------

library(caret)

# Setting the seed to 12345 for the purpose of reproducible results
###set.seed(7846449)

inTrain <- createDataPartition(final_loan_data$loan_status,p=0.70,list = FALSE)
Training <- final_loan_data[inTrain,]
Testing <- final_loan_data[-inTrain,]
table(Training$loan_status)
table(Testing$loan_status)
c(nrow(Training), nrow(Testing))

prop.table(table(Training$loan_status))

prop.table(table(Testing$loan_status))

# final_loan_data$loan_status = as.factor(final_loan_data$loan_status)
# Training$loan_status= as.factor(Training$loan_status)
# Testing$loan_status= as.factor(Testing$loan_status)

View(Testing)
str(Testing)
View(Training)
str(Training)
# ------------ Multi class RandomForest Regression --------------
# 621166 ; 10% of this is 62116 - so need to increase no of 1's from 47160 to 62116 

# library(DMwR)
# Training$loan_status <- as.factor(Training$loan_status)
# Training_p <- SMOTE(loan_status~.,data=Training,perc.over = 32,perc.under =3839)
# table(Training_p$loan_status)

library(randomForest)
set.seed(71337) 
rf <-randomForest(loan_status~.,data=Training, ntree=50) 
print(rf)

floor(sqrt(ncol(Training) - 1))


mtry <- tuneRF(Training[-7],Training$loan_status, ntreeTry=50,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

##best.m<-9
###Build model again using best mtry value
set.seed(717)
rf <-randomForest(loan_status~.,data=Training, mtry=best.m, importance=TRUE,ntree=50)
print(rf)

#Evaluate variable importance
importance(rf)
varImpPlot(rf)


###Prediction on training dataset
pred1=predict(rf,type = "prob")

library(ROCR)
perf = prediction(pred1[,2], Training$loan_status)

# 1. Area under curve
auc = performance(perf, "auc")
auc <- as.numeric(auc@y.values)
auc

# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")

# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


KS <- max(attr(pred3, 'y.values')[[1]]-attr(pred3, 'x.values')[[1]])
KS



###Prediction on Testing dataset---------
rf <-randomForest(loan_status~.,data=Testing, mtry=best.m, importance=TRUE,ntree=10)
print(rf)


pred1=predict(rf,type = "prob")

library(ROCR)
perf = prediction(pred1[,2], Testing$loan_status)

# 1. Area under curve
auc = performance(perf, "auc")
auc <- as.numeric(auc@y.values)
auc

plot(roc(pred1[,2], Testing$loan_status))

# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")

# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


KS <- max(attr(pred3, 'y.values')[[1]]-attr(pred3, 'x.values')[[1]])
KS
