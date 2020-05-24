## Data Mining Group Assignment Group 6

## Applying CART Model on the HR Attrition Data Set

install.packages("readxl")

library(readxl)

# Set working directory and import data (Target variable is "Attrition")
setwd("E:/r direct/Data Mining/Assignment")
library(readr)
HRDATA <- read.table(file.choose(), sep = ",", header = T)
summary(HRDATA)

## Remove Over18, StandardHours, EmployeeCount, EmployeeNumber columns
drops <- c("Over18", "StandardHours", "EmployeeCount", "EmployeeNumber")
HRDATA<-HRDATA[ , !(names(HRDATA) %in% drops)]

View(HRDATA)
sapply(HRDATA, function(x) sum(is.na(x)))
str(HRDATA)

## Change datatypes of integer categorial variables into factor type
HRDATA$Education<-sapply(HRDATA$Education, function(x) x= as.factor(x))
HRDATA$EnvironmentSatisfaction<-sapply(HRDATA$EnvironmentSatisfaction, function(x) x= as.factor(x))
HRDATA$JobInvolvement<-sapply(HRDATA$JobInvolvement, function(x) x= as.factor(x))
HRDATA$JobLevel<-sapply(HRDATA$JobLevel, function(x) x= as.factor(x))
HRDATA$JobSatisfaction<-sapply(HRDATA$JobSatisfaction, function(x) x= as.factor(x))
HRDATA$PerformanceRating<-sapply(HRDATA$PerformanceRating, function(x) x= as.factor(x))
HRDATA$RelationshipSatisfaction<-sapply(HRDATA$RelationshipSatisfaction, function(x) x= as.factor(x))
HRDATA$StockOptionLevel<-sapply(HRDATA$StockOptionLevel, function(x) x= as.factor(x))
HRDATA$WorkLifeBalance<-sapply(HRDATA$WorkLifeBalance, function(x) x= as.factor(x))

## Perform chi-square tests for variable independence

tbl<-table(HRDATA$Attrition,HRDATA$BusinessTravel)
tbl
chisq.test(tbl)
# X-squared = 48.365, df = 2, p-value = 3.146e-11


tbl<-table(HRDATA$Attrition,HRDATA$Department)
tbl
chisq.test(tbl) 
# X-squared = 21.592, df = 2, p-value = 2.048e-05


tbl<-table(HRDATA$Attrition,HRDATA$Education)
tbl
chisq.test(tbl) 
# X-squared = 6.1479, df = 4, p-value = 0.1884


tbl<-table(HRDATA$Attrition,HRDATA$EducationField)
tbl
chisq.test(tbl) 
# X-squared = 32.049, df = 5, p-value = 5.809e-06


tbl<-table(HRDATA$Attrition,HRDATA$EnvironmentSatisfaction)
tbl
chisq.test(tbl) 
# X-squared = 45.008, df = 3, p-value = 9.218e-10


tbl<-table(HRDATA$Attrition,HRDATA$Gender)
tbl
chisq.test(tbl) 
# X-squared = 2.3896, df = 1, p-value = 0.1221

tbl<-table(HRDATA$Attrition,HRDATA$JobInvolvement)
tbl
chisq.test(tbl) 
# X-squared = 56.984, df = 3, p-value = 2.59e-12


tbl<-table(HRDATA$Attrition,HRDATA$JobLevel)
tbl
chisq.test(tbl) 
# X-squared = 145.06, df = 4, p-value < 2.2e-16


tbl<-table(HRDATA$Attrition,HRDATA$JobRole)
tbl
chisq.test(tbl) 
# X-squared = 172.38, df = 8, p-value < 2.2e-16


tbl<-table(HRDATA$Attrition,HRDATA$JobSatisfaction)
tbl
chisq.test(tbl) 
# X-squared = 35.01, df = 3, p-value = 1.212e-07


tbl<-table(HRDATA$Attrition,HRDATA$MaritalStatus)
tbl
chisq.test(tbl) 
# X-squared = 92.327, df = 2, p-value < 2.2e-16


tbl<-table(HRDATA$Attrition,HRDATA$OverTime)
tbl
chisq.test(tbl) 
# X-squared = 176.61, df = 1, p-value < 2.2e-16


tbl<-table(HRDATA$Attrition,HRDATA$PerformanceRating)
tbl
chisq.test(tbl)
# X-squared = 0.0075887, df = 1, p-value = 0.9306


tbl<-table(HRDATA$Attrition,HRDATA$RelationshipSatisfaction)
tbl
chisq.test(tbl)
# X-squared = 10.482, df = 3, p-value = 0.01488


tbl<-table(HRDATA$Attrition,HRDATA$StockOptionLevel)
tbl
chisq.test(tbl)
# X-squared = 121.2, df = 3, p-value < 2.2e-16

tbl<-table(HRDATA$Attrition,HRDATA$WorkLifeBalance)
tbl
chisq.test(tbl)
# X-squared = 32.65, df = 3, p-value = 3.817e-07

# Education, Gender, PerformanceRating can be ignored as they are independent of Attrition with p>0.05

drops <- c("Education", "Gender", "PerformanceRating")
HRDATA<-HRDATA[ , !(names(HRDATA) %in% drops)]

### ---------------------------------------------------------------------------------- ###

# Perform logistic regression on continuous variables

lgDF<-cbind.data.frame(HRDATA$Attrition,HRDATA$Age,HRDATA$DailyRate,HRDATA$DistanceFromHome,HRDATA$HourlyRate,HRDATA$MonthlyIncome,HRDATA$MonthlyRate,HRDATA$NumCompaniesWorked,HRDATA$PercentSalaryHike,HRDATA$TotalWorkingYears,HRDATA$TrainingTimesLastYear,HRDATA$YearsAtCompany,HRDATA$YearsInCurrentRole,HRDATA$YearsSinceLastPromotion,HRDATA$YearsWithCurrManager)

colnames(lgDF)<-c("Attrition","Age","DailyRate","DistanceFromHome","HourlyRate","MonthlyIncome","MonthlyRate","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")

View(lgDF)
levels(lgDF$Attrition)<-ifelse(levels(lgDF$Attrition)=="Yes",1,0)
scaledCols<-subset(lgDF, select = c(!names(lgDF) %in% "Attrition"))
View(scaledCols)
scaledColsDF<-as.data.frame(scale(scaledCols))
finScaledColsDF<-cbind.data.frame(lgDF$Attrition,scaledColsDF)
View(finScaledColsDF)
colnames(finScaledColsDF)
colnames(finScaledColsDF)[1]<-"Attrition"
logRegModel<-glm(Attrition~.,data = finScaledColsDF,family = binomial)
summary(logRegModel)

# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -1.93771    0.06418 -30.191  < 2e-16 ***
# Age                     -0.32652    0.07558  -4.320 1.56e-05 ***
# DailyRate               -0.13792    0.05311  -2.597 0.009404 ** 
# DistanceFromHome         0.23319    0.05083   4.588 4.48e-06 ***
# HourlyRate              -0.01440    0.05272  -0.273 0.784703    
# MonthlyIncome           -0.38997    0.10297  -3.787 0.000152 ***
# MonthlyRate              0.03844    0.05312   0.724 0.469317    
# NumCompaniesWorked       0.28919    0.05591   5.173 2.31e-07 ***
# PercentSalaryHike       -0.04679    0.05396  -0.867 0.385920    
# TotalWorkingYears       -0.19501    0.13148  -1.483 0.138005    
# TrainingTimesLastYear   -0.17581    0.05388  -3.263 0.001102 ** 
# YearsAtCompany           0.35334    0.14231   2.483 0.013030 *  
# YearsInCurrentRole      -0.47771    0.09974  -4.789 1.67e-06 ***
# YearsSinceLastPromotion  0.44204    0.08224   5.375 7.65e-08 ***
# YearsWithCurrManager    -0.40862    0.09703  -4.211 2.54e-05 ***


# Attrition is independent of HourlyRate, MonthlyRate, PercentSalaryHike, TotalWorkingYears

drops <- c("HourlyRate", "MonthlyRate", "PercentSalaryHike", "TotalWorkingYears")
HRDATA<-HRDATA[ , !(names(HRDATA) %in% drops)]

### ---------------------------------------------------------------------------------- ###

=======================================================================================================================================================================
  
  ## Data Cleansing
  
  ## Form bins for continuous variables to make them categorical
  
  HRbreaks <- function(x) {
    HRbreaks_C <- cut(x, seq(min(x),max(x),(max(x) - min(x))/5), include.lowest=TRUE)
    return(HRbreaks_C) 
  }

HRDATA$AgeC <- HRbreaks(HRDATA$Age)
HRDATA$DailyRateC <- HRbreaks(HRDATA$DailyRate)
HRDATA$MonthlyIncomeC <- HRbreaks(HRDATA$MonthlyIncome)
HRDATA$DistanceFromHomeC <- HRbreaks(HRDATA$DistanceFromHome)
HRDATA$NumCompaniesWorkedC <- HRbreaks(HRDATA$NumCompaniesWorked)
HRDATA$TrainingTimesLastYearC <- HRbreaks(HRDATA$TrainingTimesLastYear)
HRDATA$YearsAtCompanyC <- HRbreaks(HRDATA$YearsAtCompany)
HRDATA$YearsInCurrentRoleC <- HRbreaks(HRDATA$YearsInCurrentRole)
HRDATA$YearsSinceLastPromotionC <- HRbreaks(HRDATA$YearsSinceLastPromotion)
HRDATA$YearsWithCurrManagerC <- HRbreaks(HRDATA$YearsWithCurrManager)

view(HRDATA)


## Neural Network model

install.packages("neuralnet")
library(neuralnet)

# Cleanup and data preparation for neural network
levels(HRDATA$BusinessTravel)<-gsub("_","",levels(HRDATA$BusinessTravel))
levels(HRDATA$BusinessTravel)<-gsub("-","",levels(HRDATA$BusinessTravel))
levels(HRDATA$Department)<-gsub(" ","",levels(HRDATA$Department))
levels(HRDATA$Department)<-gsub("&","",levels(HRDATA$Department))
levels(HRDATA$EducationField)<-gsub(" ","",levels(HRDATA$EducationField))
levels(HRDATA$JobRole)<-gsub(" ","",levels(HRDATA$JobRole))
tmpBusinessTravel<-scale(as.data.frame(model.matrix(~BusinessTravel-1, data = HRDATA)))
tmpDepartment<-scale(as.data.frame(model.matrix(~Department-1, data = HRDATA)))
tmpEducationField<-scale(as.data.frame(model.matrix(~EducationField-1, data = HRDATA)))
tmpJobRole<-scale(as.data.frame(model.matrix(~JobRole-1, data = HRDATA)))
tmpMaritalStatus<-scale(as.data.frame(model.matrix(~MaritalStatus-1, data = HRDATA)))
tmpOverTime<-scale(as.data.frame(model.matrix(~OverTime-1, data = HRDATA)))
tmpEnvironmentSatisfaction<-scale(as.data.frame(model.matrix(~EnvironmentSatisfaction-1, data = HRDATA)))
tmpJobInvolvement<-scale(as.data.frame(model.matrix(~JobInvolvement-1, data = HRDATA)))
tmpJobLevel<-scale(as.data.frame(model.matrix(~JobLevel-1, data = HRDATA)))
tmpJobSatisfaction<-scale(as.data.frame(model.matrix(~JobSatisfaction-1, data = HRDATA)))
tmpRelationshipSatisfaction<-scale(as.data.frame(model.matrix(~RelationshipSatisfaction-1, data = HRDATA)))
tmpWorkLifeBalance<-scale(as.data.frame(model.matrix(~WorkLifeBalance-1, data = HRDATA)))
tempFnlDF<-cbind.data.frame(finScaledColsDF,as.data.frame(tmpBusinessTravel),as.data.frame(tmpDepartment),as.data.frame(tmpEducationField),as.data.frame(tmpJobRole),as.data.frame(tmpMaritalStatus),as.data.frame(tmpOverTime),as.data.frame(tmpEnvironmentSatisfaction),as.data.frame(tmpJobInvolvement),as.data.frame(tmpJobLevel),as.data.frame(tmpJobSatisfaction),as.data.frame(tmpRelationshipSatisfaction),as.data.frame(tmpWorkLifeBalance))
tempFnlDF$Attrition<-ifelse(tempFnlDF$Attrition=="1",1,0)

# Dev and holdout sample split
devIndex<-sample.int(nrow(tempFnlDF),size=floor(0.7*nrow(tempFnlDF)), replace = FALSE)
nn.dev<-tempFnlDF[devIndex,]
nn.holdout<-tempFnlDF[-devIndex,]
nn.devFormula<-as.formula(paste("Attrition ~",paste(names(nn.dev[which(!names(nn.dev) %in% "Attrition")]), collapse = ' + ')))

# Build model

nn1 <- neuralnet(formula = nn.devFormula , 
                 data = nn.dev, 
                 hidden = c(5,2),
                 err.fct = "sse",
                 act.fct = "logistic",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 stepmax = 3000,
                 threshold = 0.1
)


##hidden: 5, 2    thresh: 0.1    rep: 1/1    steps:      10	min thresh: 14.95247493
##20	min thresh: 7.953559229
##30	min thresh: 4.235719546
##40	min thresh: 1.158261631
##50	min thresh: 0.7118337323
##60	min thresh: 0.5495847687
##70	min thresh: 0.5495847687
##80	min thresh: 0.5165222807
##90	min thresh: 0.4286849778
##00	min thresh: 0.4286849778
##110	min thresh: 0.155156545
##120	min thresh: 0.1505125301
##130	min thresh: 0.1490128432
##140	min thresh: 0.1490128432
##150	min thresh: 0.1076305447
##160	min thresh: 0.103948187
##170	min thresh: 0.103948187
##180	min thresh: 0.103948187
##190	min thresh: 0.103948187
##200	min thresh: 0.103948187
##210	min thresh: 0.103948187
##220	min thresh: 0.103948187
##230	min thresh: 0.1003656129
##240	min thresh: 0.1003656129
##245	error: 30.10821	time: 1.67 secs
plot(nn1)

nn.dev$Prob = nn1$net.result[[1]]
nn.dev$Class = ifelse(nn.dev$Prob>0.3,1,0)
with( nn.dev, table(Attrition, as.factor(Class)  ))

library(caret)

# Performance measures

confusionMatrix(nn.dev$Attrition, nn.dev$Class)
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction    0    1
#          0 1723   11
#          1   31  293
#                                                   
#                Accuracy : 0.9795918               
#                  95% CI : (0.9725133, 0.985253)   
#     No Information Rate : 0.8522838               
#     P-Value [Acc > NIR] : < 0.00000000000000022204
#                                                   
#                   Kappa : 0.9210941               
#  Mcnemar's Test P-Value : 0.003370431             
#                                                   
#             Sensitivity : 0.9823261               
#             Specificity : 0.9638158               
#          Pos Pred Value : 0.9936563               
#          Neg Pred Value : 0.9043210               
#              Prevalence : 0.8522838               
#          Detection Rate : 0.8372206               
#    Detection Prevalence : 0.8425656               
#       Balanced Accuracy : 0.9730710               
#                                                   
#        'Positive' Class : 0 

nn.dev$Prob = nn1$net.result[[1]]
quantile(nn.dev$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
0%                 1%                 5%                10%                25% 
0.0000003092680870 0.0000003092680870 0.0000003094342023 0.0000003374400524 0.0000005662892852 
50%                75%                90%                95%                99% 
0.0000049057291811 0.0007851366075069 0.9674019381268588 0.9997634175043724 0.9999994942111314 
100% 
0.9999996955514673 
hist(nn.dev$Prob)


library(ROCR)
pred <- ROCR::prediction(nn.dev$Prob, nn.dev$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(nn.dev$Prob, type="Gini")

with(nn.dev, table(Attrition,Class))
#          Class
# Attrition    0    1
#         0 1717    5
#         1   50  286

KS
# 0.8532955865
auc
# 0.9253809247
gini
# 0.8548067901

# deciling

nn.dev$deciles <- decile(nn.dev$Prob)
tmp_DT = data.table(nn.dev)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_attr = sum(Attrition), 
  cnt_non_attr = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
tmp_DT = data.table(nn.dev)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_attr = sum(Attrition), 
  cnt_non_attr = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]
rank$attrrate <- round (rank$cnt_attr / rank$cnt,2);
rank$cum_attr <- cumsum(rank$cnt_attr)
rank$cum_non_attr <- cumsum(rank$cnt_non_attr)
rank$cum_rel_attr <- round(rank$cum_attr / sum(rank$cnt_attr),2);
rank$cum_rel_non_attr <- round(rank$cum_non_attr / sum(rank$cnt_non_attr),2);
rank$ks <- abs(rank$cum_rel_attr - rank$cum_rel_non_attr);
rank$attrrate <- percent(rank$attrrate)
rank$cum_rel_attr <- percent(rank$cum_rel_attr)
rank$cum_rel_non_attr <- percent(rank$cum_rel_non_attr)
View(rank)

# holdout sample
compute.output = compute(nn1, nn.holdout[,-1])
nn.holdout$Predict.score = compute.output$net.result
nn.holdout$Class = ifelse(nn.holdout$Predict.score>0.5,1,0)
nn.holdout$deciles <- decile(nn.holdout$Predict.score)

tmp_DT = data.table(nn.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_attr = sum(Attrition), 
  cnt_non_attr = sum(Attrition == 0)) , 
  by=deciles][order(-deciles)]

h_rank$attrrate <- round (h_rank$cnt_attr / h_rank$cnt,2);
h_rank$cum_attr <- cumsum(h_rank$cnt_attr)
h_rank$cum_non_attr <- cumsum(h_rank$cnt_non_attr)
h_rank$cum_rel_attr <- round(h_rank$cum_attr / sum(h_rank$cnt_attr),2);
h_rank$cum_rel_non_attr <- round(h_rank$cum_non_attr / sum(h_rank$cnt_non_attr),2);
h_rank$ks <- abs(h_rank$cum_rel_attr - h_rank$cum_rel_non_attr);
h_rank$attrrate <- percent(h_rank$attrrate)
h_rank$cum_rel_attr <- percent(h_rank$cum_rel_attr)
h_rank$cum_rel_non_attr <- percent(h_rank$cum_rel_non_attr)
View(h_rank)

# Performance measures

confusionMatrix(nn.holdout$Attrition, nn.holdout$Class)
# Confusion Matrix and Statistics
#           Reference
# Prediction   0   1
#          0 715  29
#          1  40  98                                                
#                Accuracy : 0.9217687             
#                  95% CI : (0.9020346, 0.9386243)
#     No Information Rate : 0.8560091             
#     P-Value [Acc > NIR] : 0.000000001398487                                                   
#                   Kappa : 0.6936853             
#  Mcnemar's Test P-Value : 0.2286443                                                             
#             Sensitivity : 0.9470199             
#             Specificity : 0.7716535             
#          Pos Pred Value : 0.9610215             
#          Neg Pred Value : 0.7101449             
#              Prevalence : 0.8560091             
#          Detection Rate : 0.8106576             
#    Detection Prevalence : 0.8435374             
#       Balanced Accuracy : 0.8593367                                                           
#        'Positive' Class : 0 

library(ROCR)
pred <- ROCR::prediction(nn.holdout$Predict.score, nn.holdout$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(nn.holdout$Predict.score, type="Gini")

with(nn.holdout, table(Attrition,Class))
#          Class
# Attrition   0   1
#         0 715  29
#         1  40  98

KS
# 0.676542777
auc
# 0.8551503818
gini
# 0.8565165299

####---------------CART MODEL IMPLEMENTATION-----------######
## Splitting 70:30 samples

devIndex<-sample.int(nrow(HRDATA),size=floor(0.7*nrow(HRDATA)), replace = FALSE)
CART.dev<-HRDATA[devIndex,]
CART.holdout<-HRDATA[-devIndex,]

## installing rpart package for CART
install.packages("rpart")
install.packages("rpart.plot")

## loading the library
library(rpart)
library(rpart.plot)

## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)

## Run cart on dev sample

m1 <- rpart(formula = Attrition ~ AgeC +  BusinessTravel
            + DailyRateC + Department + DistanceFromHomeC + EducationField 
            + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobRole
            + JobSatisfaction + MaritalStatus + MonthlyIncomeC  + NumCompaniesWorkedC + OverTime + RelationshipSatisfaction + StockOptionLevel  + TrainingTimesLastYearC + WorkLifeBalance + YearsAtCompanyC  + YearsInCurrentRoleC + YearsSinceLastPromotionC + YearsWithCurrManagerC, data = CART.dev, method = "class", control = r.ctrl)
m1

## Display tree

prp(m1)
install.packages("rattle")
install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
install.packages("RColorBrewer")

library(rattle)
library(RColorBrewer)
fancyRpartPlot(m1)

## Prune tree

printcp(m1)
#          CP nsplit rel error  xerror     xstd
# 1 0.0393939      0   1.00000 1.00000 0.050442
# 2 0.0166667      3   0.88182 0.88182 0.047899
# 3 0.0040404      5   0.84848 0.90909 0.048510
# 4 0.0030303      8   0.83636 0.91515 0.048644
# 5 0.0020202      9   0.83333 0.93333 0.049041
# 6 0.0000000     12   0.82727 0.92121 0.048777

ptree<- prune(m1,cp=  m1$cptable[which.min(m1$cptable[,"xerror"]),"CP"])
printcp(ptree)


## Scoring and performance measure on dev sample

## Scoring syntax
CART.dev$predict.class <- predict(ptree, CART.dev, type="class")
CART.dev$predict.score <- predict(ptree, CART.dev)

install.packages("caret")
library(caret)
confusionMatrix(CART.dev$Attrition,CART.dev$predict.class)
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   No  Yes
#        No  1684   44
#        Yes  247   83
#                                           
#                Accuracy : 0.8586          
#                  95% CI : (0.8428, 0.8734)
#     No Information Rate : 0.9383          
#     P-Value [Acc > NIR] : 1               
#                                           
#                   Kappa : 0.3009          
#  Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.8721          
#             Specificity : 0.6535          
#          Pos Pred Value : 0.9745          
#          Neg Pred Value : 0.2515          
#              Prevalence : 0.9383          
#          Detection Rate : 0.8183          
#    Detection Prevalence : 0.8397          
#       Balanced Accuracy : 0.7628          
#                                           
#      'Positive' Class : No install.packages("ROCR")

library(ROCR)
pred <- prediction(CART.dev$predict.score[,2], CART.dev$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

install.packages("ineq")

library(ineq)
gini = ineq(CART.dev$predict.score[,2], type="Gini")

with(CART.dev, table(Attrition, predict.class))
#          predict.class
# Attrition   No  Yes
#       No  1684   44
#       Yes  247   83
auc
# 0.6729342
KS
# 0.292319
gini
# 0.2904085


## Perform deciling

decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}


## deciling
CART.dev$deciles <- decile(CART.dev$predict.score[,2])

## Ranking code
install.packages("data.table")

library(data.table)
library(scales)

CART.dev$scode<-ifelse(CART.dev$Attrition=="Yes",1,0)

tmp_DT = data.table(CART.dev)
rank <- tmp_DT[, list(
  cnt = length(scode), 
  cnt_attr = sum(scode), 
  cnt_non_attr = sum(scode == 0)) , 
  by=deciles][order(-deciles)]
rank$attrrate <- round (rank$cnt_attr / rank$cnt,2);
rank$cum_attr <- cumsum(rank$cnt_attr)
rank$cum_non_attr <- cumsum(rank$cnt_non_attr)
rank$cum_rel_attr <- round(rank$cum_attr / sum(rank$cnt_attr),2);
rank$cum_rel_non_attr <- round(rank$cum_non_attr / sum(rank$cnt_non_attr),2);
rank$ks <- abs(rank$cum_rel_attr - rank$cum_rel_non_attr);
rank$attrrate <- percent(rank$attrrate)
rank$cum_rel_attr <- percent(rank$cum_rel_attr)
rank$cum_rel_non_attr <- percent(rank$cum_rel_non_attr)
View(rank)

## Scoring Holdout sample
CART.holdout$predict.class <- predict(ptree, CART.holdout, type="class")
CART.holdout$predict.score <- predict(ptree, CART.holdout)

confusionMatrix(CART.holdout$Attrition,CART.holdout$predict.class)
# Confusion Matrix and Statistics
# 
#           Reference
# Prediction  No Yes
#        No  720  18
#        Yes 111  33
#                                           
#                Accuracy : 0.8537          
#                  95% CI : (0.8287, 0.8764)
#     No Information Rate : 0.9422          
#     P-Value [Acc > NIR] : 1               
#                                           
#                   Kappa : 0.2767          
#  Mcnemar's Test P-Value : 5.489e-16       
#                                           
#             Sensitivity : 0.8664          
#             Specificity : 0.6471          
#          Pos Pred Value : 0.9756          
#          Neg Pred Value : 0.2292          
#              Prevalence : 0.9422          
#          Detection Rate : 0.8163          
#    Detection Prevalence : 0.8367          
#       Balanced Accuracy : 0.7567          
#                                           
#        'Positive' Class : No  

pred <- prediction(CART.holdout$predict.score[,2], CART.holdout$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(CART.holdout$predict.score[,2], type="Gini")

with(CART.holdout, table(Attrition, predict.class))
#          predict.class
# Attrition  No Yes
#       No  720  18
#       Yes 111  33
auc
# 0.6903606
KS
# 0.3224932
gini
# 0.2866926


CART.holdout$deciles <- decile(CART.holdout$predict.score[,2])

CART.holdout$scode<-ifelse(CART.holdout$Attrition=="Yes",1,0)

tmp_DT = data.table(CART.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(scode), 
  cnt_attr = sum(scode), 
  cnt_non_attr = sum(scode == 0)) , 
  by=deciles][order(-deciles)]
h_rank$attrrate <- round (h_rank$cnt_attr / h_rank$cnt,2);
h_rank$cum_attr <- cumsum(h_rank$cnt_attr)
h_rank$cum_non_attr <- cumsum(h_rank$cnt_non_attr)
h_rank$cum_rel_attr <- round(h_rank$cum_attr / sum(h_rank$cnt_attr),2);
h_rank$cum_rel_non_attr <- round(h_rank$cum_non_attr / sum(h_rank$cnt_non_attr),2);
h_rank$ks <- abs(h_rank$cum_rel_attr - h_rank$cum_rel_non_attr);
h_rank$attrrate <- percent(h_rank$attrrate)
h_rank$cum_rel_attr <- percent(h_rank$cum_rel_attr)
h_rank$cum_rel_non_attr <- percent(h_rank$cum_rel_non_attr)
View(h_rank)
