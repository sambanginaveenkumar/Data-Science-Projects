##Attaching Data
dataset = read.csv(file.choose(new = FALSE))
attach(dataset)
str(dataset)
summary(dataset)
sapply(dataset, function(x) sum(is.na(x)))


Data_plot <- dataset[,c(-3,-4)]
boxplot(Data_plot)
names(Data_plot)
#From the boxplots, looks like 1,4,5 and 6 have similar scales and others have lower scales
#Create a panel of boxplots 
library(psych)
library(ggplot2)
require(reshape2)
df <- melt(Data_plot, id.var = "Churn") 
head(df)
require(ggplot2)
ggplot(data = df, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Churn)) + 
  facet_wrap( ~ variable, scales="free") +
  theme_bw()


## Converting Integer Data Into Categorical Data
dataset$Churn = factor(dataset$Churn, levels = c(0,1), labels= c(0,1))
dataset$ContractRenewal = factor(dataset$ContractRenewal, levels = c(0,1), labels = c(0,1))
dataset$DataPlan = factor(dataset$DataPlan, levels = c(0,1), labels = c(0,1))
str(dataset)
var(dataset$Churn)
sd(dataset$Churn)

##splitting Data Into Train & Test With 80% for TRaining & 20% for Test
library(caTools)
set.seed(123)
split1 = sample.split(dataset$Churn, SplitRatio = 0.8)
training_set_1 = subset(dataset,split1 == TRUE)
test_set_1 = subset(dataset, split1 == FALSE)
##Normalising Data
training_set_1[,5:11] = scale(training_set_1[,5:11])
training_set_1[,2] = scale(training_set_1[,2])
test_set_1[,5:11] = scale(test_set_1[,5:11])
test_set_1[,2] = scale(test_set_1[,2])
##Building A Classification Model
classifier1 = glm(training_set_1$Churn~., data = training_set_1, family = binomial())
summary(classifier1)
## likelihood ratio test and McFadden Rsquare test
library(lmtest)
library(pscl)

lrtest(classifier1)
pR2(classifier1)
##odds and probability of coefficients
exp(coef(classifier1))
probability1 = exp(coef(classifier1))/(1+exp(coef(classifier1)))
probability1
##Performance/ Predicting of Classifier on test data
pred_prob1 = predict(classifier1, type = 'response', newdata = test_set_1[-1])
y1_prediction = ifelse(pred_prob1>0.2,1,0)
table(Actual = test_set_1[,1],Predicted = y1_prediction)
