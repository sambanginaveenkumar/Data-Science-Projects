rm(list=ls())
setwd("E:/r direct/smdm/SMDM Assignment")

#### reading the data file
titandata<-read.csv("titaninsurance.csv",header=TRUE)
###Summarizing the data to know the min 1st qu median mean 3rd qur max  
summary(titandata)

## Histogram for before and after payment scheme introductions 
par(mfrow=c(1,2))
hist(titandata$Old_Scheme,main='OldScheme',xlab = "POUNDS(000)",ylab = "SalesForce",col = "turquoise")
hist(titandata$New_Scheme,main='NewScheme',xlab = "POUNDS(000)",ylab = "SalesForce",col = "turquoise1")

##Boxplot for before and after payment scheme introductions 

y1<-titandata$Old_Scheme
y2<-titandata$New_Scheme

library(reshape)
data<-data.frame(y1,y2)
meltData <-melt(data)
boxplot(data=meltData, value~variable, na.rm=TRUE,main='Boxplot for Comparision of Old and New Scheme',xlab = "POUNDS(000)",ylab = "SalesForce",col = "turquoise1"
        )


var(titandata$New_Scheme)-var(titandata$Old_Scheme)

##Pooled sample of two tail teset as the variances and means are equal
t.test(titandata$New_Scheme,titandata$Old_Scheme,
       mu=0,paired = TRUE,var.equal = TRUE)

##Pooled sample of one tail teset as the variances and means are equal 
t.test(titandata$New_Scheme,titandata$Old_Scheme,alternative = c("greater"),
       mu=0,paired = TRUE,var.equal = TRUE)


##Pooled sample of average output must increase by £5000.
t.test(titandata$New_Scheme,titandata$Old_Scheme,mu=5,alternative = c("greater"),
       var.equal=TRUE, paired=TRUE)
##Based on the above result(P value :06499)
##probabilty of type 1 error is 0.6499 so we don't have enough evidence from the 
#sample the average output is increaed by 5000 POUNDS.  

##probabilty of type 2 error is 1-power(1-0.25==>0.75)

##Power of the Test where the average output must increase by £5000
power.t.test(n = 30, delta = 4, sd = 22.33, sig.level = 0.05,
             power = NULL,
             type = c("paired"),
             alternative = c("one.sided"),
             strict = FALSE, tol = .Machine$double.eps^0.25)

##We need sample size 406 to get probabilty of type 1 and type 2 error are equal 
power.t.test(power = 0.95, delta = 4, sd = 22.33, sig.level = 0.05,
             type = c("paired"),
             alternative = c("two.sided"),
             strict = FALSE, tol = .Machine$double.eps^0.25)
