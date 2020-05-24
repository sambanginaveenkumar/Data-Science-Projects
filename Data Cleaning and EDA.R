###install.packages("VIM")
###install.packages("mice")

setwd("E:/r direct/Capstone Project/Project")
library(readr)
LOANDATA <- read.csv("loan.csv",header=TRUE)
summary(LOANDATA)
## to know th number of missing values in each column
sort(sapply(LOANDATA, function(x) sum(is.na(x))), decreasing=TRUE)

str(LOANDATA)

##Removing/Drop the columns from the dataset having more than 80% null values

miss <- c()
for(i in 1:ncol(LOANDATA)) {
  if(length(which(is.na(LOANDATA[,i]))) > 0.8*nrow(LOANDATA)) miss <- append(miss,i) 
}
LOANDATA2 <- LOANDATA[,-miss]
sort(sapply(LOANDATA2, function(x) sum(is.na(x))), decreasing=TRUE)

## Data Contains 887379 unique records of cusomers  

names<-as.data.frame(names(LOANDATA2))
Uniquedata<-as.data.frame(sort(rapply(LOANDATA2,function(x)length(unique(x)))),decreasing=TRUE)

match(LOANDATA2$loan_amnt==LOANDATA2$funded_amnt)




##Handling null values with mice package for contineous varables
###need to remove the factor variables and then impute it for contineous values and  then work on factor variables
library(mice)
##Number of rows and columns having missing values
md.pattern(LOANDATA2)
library(VIM)
mice_plot <- aggr(LOANDATA2, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))