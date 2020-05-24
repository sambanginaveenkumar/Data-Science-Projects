# Over18, StandardHours, EmployeeCount, EmployeeNumber have same row values so can be ignored

# Perform chisquare test on categorical and ordinal variables
install.packages("neuralnet")
install.packages("ROCR")
install.packages("ineq")
library(neuralnet)
 setwd("E:/r direct/Data Mining/Assignment")
 library(readr)
  hrdata <- read.table(file.choose(), sep = ",", header = T)
##Parsed with column specification:
  cols(
    .default = col_integer(),
    Attrition = col_character(),
    BusinessTravel = col_character(),
    Department = col_character(),
    EducationField = col_character(),
    Gender = col_character(),
    JobRole = col_character(),
    MaritalStatus = col_character(),
    Over18 = col_character(),
    OverTime = col_character()
  )
  str(hrdata)
  summary(hrdata)
##Droping the columns   
  drops <- c("Over18", "StandardHours", "EmployeeCount", "EmployeeNumber")
  hrdata<-hrdata[ , !(names(hrdata) %in% drops)]
  
  View(hrdata)
  sapply(hrdata, function(x) sum(is.na(x)))
  str(hrdata)
  

 View(hrdata)
 tbl<-table(hrdata$Attrition,hrdata$BusinessTravel)
 tbl
##OUTPUT
Non-Travel Travel_Frequently Travel_Rarely
No         276               416          1774
Yes         24               138           312
 chisq.test(tbl)
 ##OUTPUT
Pearson's Chi-squared test

data:  tbl
X-squared = 48.365, df = 2, p-value = 3.146e-11

# Attrition is dependent on BusinessTravel

 tbl<-table(hrdata$Attrition,hrdata$Department)
 tbl

Human Resources Research & Development Sales
No              102                   1656   708
Yes              24                    266   184
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 21.592, df = 2, p-value = 2.048e-05

# Attrition is dependent on Department

 tbl<-table(hrdata$Attrition,hrdata$Education)
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 6.1479, df = 4, p-value = 0.1884

# Attrition is independent on Education

 tbl<-table(hrdata$Attrition,hrdata$EducationField)
 tbl

Human Resources Life Sciences Marketing Medical Other Technical Degree
No               40          1034       248     802   142              200
Yes              14           178        70     126    22               64
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 32.049, df = 5, p-value = 5.809e-06

# Attrition is dependent on EducationField

 tbl<-table(hrdata$Attrition,hrdata$EnvironmentSatisfaction)
 tbl

1   2   3   4
No  424 488 782 772
Yes 144  86 124 120
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 45.008, df = 3, p-value = 9.218e-10

# Attrition is dependent on EnvironmentSatisfaction

 tbl<-table(hrdata$Attrition,hrdata$Gender)
 tbl

Female Male
No    1002 1464
Yes    174  300
 chisq.test(tbl)

Pearson's Chi-squared test with Yates' continuity correction

data:  tbl
X-squared = 2.3896, df = 1, p-value = 0.1221

# Attrition is independent of Gender

 tbl<-table(hrdata$Attrition,hrdata$JobInvolvement)
 tbl

1    2    3    4
No   110  608 1486  262
Yes   56  142  250   26
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 56.984, df = 3, p-value = 2.59e-12

# Attrition is dependent on JobInvolvement

 tbl<-table(hrdata$Attrition,hrdata$JobLevel)
 tbl

1   2   3   4   5
No  800 964 372 202 128
Yes 286 104  64  10  10
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 145.06, df = 4, p-value < 2.2e-16

# Attrition is dependent on JobLevel

 tbl<-table(hrdata$Attrition,hrdata$JobRole)
 tbl

Healthcare Representative Human Resources Laboratory Technician Manager
No                        244              80                   394     194
Yes                        18              24                   124      10

Manufacturing Director Research Director Research Scientist Sales Executive
No                     270               156                490             538
Yes                     20                 4                 94             114

Sales Representative
No                   100
Yes                   66
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 172.38, df = 8, p-value < 2.2e-16

# Attrition is dependent on JobRole

 tbl<-table(hrdata$Attrition,hrdata$JobSatisfaction)
 tbl

1   2   3   4
No  446 468 738 814
Yes 132  92 146 104
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 35.01, df = 3, p-value = 1.212e-07

# Attrition is dependent on JobSatisfaction

 tbl<-table(hrdata$Attrition,hrdata$MaritalStatus)
 tbl

Divorced Married Single
No       588    1178    700
Yes       66     168    240
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 92.327, df = 2, p-value < 2.2e-16

# Attrition is dependent on MaritalStatus

 tbl<-table(hrdata$Attrition,hrdata$OverTime)
 tbl

No  Yes
No  1888  578
Yes  220  254
 chisq.test(tbl)

Pearson's Chi-squared test with Yates' continuity correction

data:  tbl
X-squared = 176.61, df = 1, p-value < 2.2e-16

# Attrition is dependent on OverTime

 tbl<-table(hrdata$Attrition,hrdata$PerformanceRating)
 tbl

3    4
No  2088  378
Yes  400   74
 chisq.test(tbl)

Pearson's Chi-squared test with Yates' continuity correction

data:  tbl
X-squared = 0.0075887, df = 1, p-value = 0.9306

# Attrition is independent on PerformanceRating

 tbl<-table(hrdata$Attrition,hrdata$RelationshipSatisfaction)
 tbl

1   2   3   4
No  438 516 776 736
Yes 114  90 142 128
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 10.482, df = 3, p-value = 0.01488

# Attrition is dependent on RelationshipSatisfaction

 tbl<-table(hrdata$Attrition,hrdata$StockOptionLevel)
 tbl

0    1    2    3
No   954 1080  292  140
Yes  308  112   24   30
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 121.2, df = 3, p-value < 2.2e-16

# Attrition is dependent on StockOptionLevel

 tbl<-table(hrdata$Attrition,hrdata$WorkLifeBalance)
 tbl

1    2    3    4
No   110  572 1532  252
Yes   50  116  254   54
 chisq.test(tbl)

Pearson's Chi-squared test

data:  tbl
X-squared = 32.65, df = 3, p-value = 3.817e-07

# Attrition is dependent on WorkLifeBalance

# Hence Education, Gender, PerformanceRating can be ignored

drops <- c("Education", "Gender", "PerformanceRating")
hrdata<-hrdata[ , !(names(hrdata) %in% drops)]

=======================================================================================================================================================================

# Perform logistic regression on continuous variables

 lgDF<-cbind.data.frame(hrdata$Attrition,hrdata$Age,hrdata$DailyRate,hrdata$DistanceFromHome,hrdata$HourlyRate,hrdata$MonthlyIncome,hrdata$MonthlyRate,hrdata$NumCompaniesWorked,hrdata$PercentSalaryHike,hrdata$TotalWorkingYears,hrdata$TrainingTimesLastYear,hrdata$YearsAtCompany,hrdata$YearsInCurrentRole,hrdata$YearsSinceLastPromotion,hrdata$YearsWithCurrManager)

 colnames(lgDF)<-c("Attrition","Age","DailyRate","DistanceFromHome","HourlyRate","MonthlyIncome","MonthlyRate","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")

 View(lgDF);
 levels(lgDF$Attrition)<-ifelse(levels(lgDF$Attrition)=="Yes",1,0)
 scaledCols<-subset(lgDF, select = c(!names(lgDF) %in% "Attrition"))
 View(scaledCols)
 scaledColsDF<-scale(scaledCols)
 scaledColsDF<-as.data.frame(scale(scaledCols))
 finScaledColsDF<-cbind.data.frame(lgDF$Attrition,scaledColsDF)
 View(finScaledColsDF)
 colnames(finScaledColsDF)
[1] "lgDF$Attrition"          "Age"                     "DailyRate"              
[4] "DistanceFromHome"        "HourlyRate"              "MonthlyIncome"          
[7] "MonthlyRate"             "NumCompaniesWorked"      "PercentSalaryHike"      
[10] "TotalWorkingYears"       "TrainingTimesLastYear"   "YearsAtCompany"         
[13] "YearsInCurrentRole"      "YearsSinceLastPromotion" "YearsWithCurrManager"   
 colnames(finScaledColsDF)[1]<-"Attrition"
 logRegModel<-glm(Attrition~.,data = finScaledColsDF,family = binomial)
 summary(logRegModel)

Call:
glm(formula = Attrition ~ ., family = binomial, data = finScaledColsDF)

Deviance Residuals: 
Min       1Q   Median       3Q      Max  
-1.2445  -0.6468  -0.4612  -0.2605   3.1140  

Coefficients:
Estimate Std. Error z value Pr(|z|)    
(Intercept)             -1.93771    0.06418 -30.191  < 2e-16 ***
Age                     -0.32652    0.07558  -4.320 1.56e-05 ***
DailyRate               -0.13792    0.05311  -2.597 0.009404 ** 
DistanceFromHome         0.23319    0.05083   4.588 4.48e-06 ***
HourlyRate              -0.01440    0.05272  -0.273 0.784703    
MonthlyIncome           -0.38997    0.10297  -3.787 0.000152 ***
MonthlyRate              0.03844    0.05312   0.724 0.469317    
NumCompaniesWorked       0.28919    0.05591   5.173 2.31e-07 ***
PercentSalaryHike       -0.04679    0.05396  -0.867 0.385920    
TotalWorkingYears       -0.19501    0.13148  -1.483 0.138005    
TrainingTimesLastYear   -0.17581    0.05388  -3.263 0.001102 ** 
YearsAtCompany           0.35334    0.14231   2.483 0.013030 *  
YearsInCurrentRole      -0.47771    0.09974  -4.789 1.67e-06 ***
YearsSinceLastPromotion  0.44204    0.08224   5.375 7.65e-08 ***
YearsWithCurrManager    -0.40862    0.09703  -4.211 2.54e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2597.2  on 2939  degrees of freedom
Residual deviance: 2330.1  on 2925  degrees of freedom
AIC: 2360.1

Number of Fisher Scoring iterations: 5

# Attrition is independent of HourlyRate, MonthlyRate, PercentSalaryHike, TotalWorkingYears and hence can be ignored

=======================================================================================================================================================================

## Data Cleansing

 finScaledColsDFClean<-finScaledColsDF[ , !(names(finScaledColsDF) %in% c("HourlyRate","MonthlyRate","PercentSalaryHike","TotalWorkingYears"))]

 setwd("E:/r direct/Data Mining/Assignment")
 hrdataNew<-read.csv("HR_Employee_Attrition_Data.csv",header = TRUE)
 levels(hrdataNew$Department)
[1] "Human Resources"        "Research & Development" "Sales"                 
 levels(hrdataNew$BusinessTravel)
[1] "Non-Travel"        "Travel_Frequently" "Travel_Rarely"    
 levels(hrdataNew$BusinessTravel)<-gsub("_","",levels(hrdataNew$BusinessTravel))
 levels(hrdataNew$BusinessTravel)
[1] "Non-Travel"       "TravelFrequently" "TravelRarely"    
 levels(hrdataNew$BusinessTravel)<-gsub("-","",levels(hrdataNew$BusinessTravel))
 levels(hrdataNew$BusinessTravel)
[1] "NonTravel"        "TravelFrequently" "TravelRarely"    
 levels(hrdataNew$Department)
[1] "Human Resources"        "Research & Development" "Sales"                 
 levels(hrdataNew$Department)<-gsub(" ","",levels(hrdataNew$Department))
 levels(hrdataNew$Department)<-gsub("&","",levels(hrdataNew$Department))
 levels(hrdataNew$Department)
[1] "HumanResources"      "ResearchDevelopment" "Sales"              
 levels(hrdataNew$EducationField)
[1] "Human Resources"  "Life Sciences"    "Marketing"        "Medical"         
[5] "Other"            "Technical Degree"
 levels(hrdataNew$EducationField)<-gsub(" ","",levels(hrdataNew$EducationField))
 levels(hrdataNew$JobRole)
[1] "Healthcare Representative" "Human Resources"           "Laboratory Technician"    
[4] "Manager"                   "Manufacturing Director"    "Research Director"        
[7] "Research Scientist"        "Sales Executive"           "Sales Representative"     
 levels(hrdataNew$JobRole)<-gsub(" ","",levels(hrdataNew$JobRole))

 

## Dummy variables
 tmpBusinessTravel<-as.data.frame(model.matrix(~BusinessTravel-1, data = hrdataNew))
 tmpDepartment<-as.data.frame(model.matrix(~Department-1, data = hrdataNew))
 tmpEducationField<-as.data.frame(model.matrix(~EducationField-1, data = hrdataNew))
 tmpGender<-as.data.frame(model.matrix(~Gender-1, data = hrdataNew))
 tmpJobRole<-as.data.frame(model.matrix(~JobRole-1, data = hrdataNew))
 tmpMaritalStatus<-as.data.frame(model.matrix(~MaritalStatus-1, data = hrdataNew))
 tmpOverTime<-as.data.frame(model.matrix(~OverTime-1, data = hrdataNew))

## Combining and scaling data

 tempFnlDF<-cbind.data.frame(finScaledColsDFClean,as.data.frame(tmpBusinessTravel),as.data.frame(tmpDepartment),as.data.frame(tmpEducationField),as.data.frame(tmpJobRole),as.data.frame(tmpMaritalStatus),as.data.frame(tmpOverTime),as.data.frame(hrdata$EnvironmentSatisfaction),as.data.frame(hrdataNew$JobInvolvement),as.data.frame(hrdataNew$JobLevel),as.data.frame(hrdataNew$JobSatisfaction),as.data.frame(hrdataNew$RelationshipSatisfaction),as.data.frame(hrdataNew$WorkLifeBalance))
 colnames(tempFnlDF)[38:43]<-c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","RelationshipSatisfaction","WorkLifeBalance")
 tempFnlDF$Attrition<-ifelse(tempFnlDF$Attrition=="1",1,0)
 tempFnlDF$EnvironmentSatisfaction<-scale(tempFnlDF$EnvironmentSatisfaction)
 tempFnlDF$JobInvolvement<-scale(tempFnlDF$JobInvolvement)
 tempFnlDF$JobLevel<-scale(tempFnlDF$JobLevel)
 tempFnlDF$JobSatisfaction<-scale(tempFnlDF$JobSatisfaction)
 tempFnlDF$RelationshipSatisfaction<-scale(tempFnlDF$RelationshipSatisfaction)
 tempFnlDF$WorkLifeBalance<-scale(tempFnlDF$WorkLifeBalance)
 View(tempFnlDF)

## Splitting 70:30 samples

 devIndex<-sample.int(nrow(tempFnlDF),size=floor(0.7*nrow(tempFnlDF)), replace = FALSE)
 nn.dev<-tempFnlDF[devIndex,]
 nn.holdout<-tempFnlDF[-devIndex,]
 nn.devFormula<-as.formula(paste("Attrition ~",paste(names(nn.dev[which(!names(nn.dev) %in% "Attrition")]), collapse = ' + ')))


## Run neural network on dev sample

 nn1 <- neuralnet(formula = nn.devFormula , 
                  data = nn.dev, 
                  hidden = c(5,2),
                  err.fct = "sse",
                  act.fct = "logistic",
                  linear.output = FALSE,
                  lifesign = "full",
                  lifesign.step = 10,
                  threshold = 0.1
 )
hidden: 5, 2    thresh: 0.1    rep: 1/1    steps:      10	min thresh: 7.962610684
20	min thresh: 2.330003935
30	min thresh: 2.330003935
40	min thresh: 0.8125408943
50	min thresh: 0.801716401
60	min thresh: 0.801716401
70	min thresh: 0.7949520786
80	min thresh: 0.7247791261
90	min thresh: 0.7247791261
100	min thresh: 0.7247791261
110	min thresh: 0.7247791261
120	min thresh: 0.6996788263
130	min thresh: 0.6901661966
140	min thresh: 0.6774233973
150	min thresh: 0.6720661892
160	min thresh: 0.6078968545
170	min thresh: 0.2478975067
180	min thresh: 0.2478975067
190	min thresh: 0.2410880202
200	min thresh: 0.1370429659
210	min thresh: 0.1270749086
212	error: 41.77089	time: 0.59 secs
 plot(nn1)
 nn.dev$Prob = nn1$net.result[[1]]
 quantile(nn.dev$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
0%                 1%                 5%                10%                25% 
0.0000003092680870 0.0000003092680870 0.0000003094342023 0.0000003374400524 0.0000005662892852 
50%                75%                90%                95%                99% 
0.0000049057291811 0.0007851366075069 0.9674019381268588 0.9997634175043724 0.9999994942111314 
100% 
0.9999996955514673 
 hist(nn.dev$Prob)

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

 nn.dev$deciles <- decile(nn.dev$Prob)
library(data.table)
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

## Score holdout sample using dev model

 compute.output = compute(nn1, nn.holdout[,-1])
 nn.holdout$Predict.score = compute.output$net.result
 View(nn.holdout)
 quantile(nn.holdout$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
0%                 1%                 5%                10%                25% 
0.0000003092680870 0.0000003092680870 0.0000003092685095 0.0000003426461660 0.0000006236772594 
50%                75%                90%                95%                99% 
0.0000065692092188 0.0008319238279216 0.9521030768431875 0.9998919534221254 0.9999994075270046 
100% 
0.9999996955514673 
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
 
 
 library(scales)
 h_rank$attrrate <- percent(h_rank$attrrate)
 h_rank$cum_rel_attr <- percent(h_rank$cum_rel_attr)
 h_rank$cum_rel_non_attr <- percent(h_rank$cum_rel_non_attr)
 View(h_rank)

## Split classes
library(caret)
 nn.dev$Class = ifelse(nn.dev$Prob>0.3,1,0)
 with( nn.dev, table(Attrition, as.factor(Class)  ))

Attrition    0    1
0 1726   10
1   74  248

## Confusion matrix

confusionMatrix(nn.dev$Attrition, nn.dev$Class)

confusionMatrix(
  factor(nn.dev$Attrition, levels = 1:1058),
  factor(nn.dev$Class, levels = 1:1058)
)

Confusion Matrix and Statistics

Reference
Prediction    0    1
0 1726   10
1   74  248

Accuracy : 0.9591837               
95% CI : (0.9497138, 0.9673151)  
No Information Rate : 0.8746356               
P-Value [Acc  NIR] : < 0.00000000000000022204

Kappa : 0.8317528               
Mcnemar's Test P-Value : 0.000000000006248598    

Sensitivity : 0.9588889               
Specificity : 0.9612403               
Pos Pred Value : 0.9942396               
Neg Pred Value : 0.7701863               
Prevalence : 0.8746356               
Detection Rate : 0.8386783               
Detection Prevalence : 0.8435374               
Balanced Accuracy : 0.9600646               

'Positive' Class : 0  

## Error computation

 sum((nn.dev$Attrition - nn.dev$Prob)^2)/2
[1] 41.77088994

## Plot true positive and false positive rates
 
 library(ROCR)
 pred <- prediction(nn.dev$Prob, nn.dev$Attrition)
 perf <- performance(pred, "tpr", "fpr")
 plot(perf)

 KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
 auc <- performance(pred,"auc"); 
 auc <- as.numeric(auc@y.values)
 
## Other performance measures

 KS
[1] 0.7644259667
 auc
[1] 0.8796404957

 library(ineq)
 gini = ineq(nn.dev$Prob, type="Gini")
 gini
[1] 0.8730020322

