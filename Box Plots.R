setwd("E:/r direct/Capstone Project/Project")

Loan1<-read.csv("loan.csv",header=TRUE)
loan_data = Loan1

#Process Missing Columns(with no observations at all)
ncol=rep(nrow(loan_data) ,each=ncol(loan_data))
missingdata=as.data.frame(cbind(colnames=names(loan_data),ncol,
                                nmsg=as.integer(as.character(as.vector(apply(loan_data, 2, function(x) length(which(is.na(x)))))))))

# for each colunm how many obeservations are missing
missingdata$nmsg
# above commmand is not showing emp_length, earliest_cr_line,last_pymt_d,next_pymt_d,last_credit_pull_d
# as columns with NAs though they have NAs in the Data ...why??

missingdata$nmsg=as.numeric(levels(missingdata$nmsg))[missingdata$nmsg]
missingdata$nmsg
missingdata=cbind(missingdata,percmissing=as.integer(missingdata$nmsg/ncol*100))
missingdata$percmissing
drops=as.character(subset(missingdata,missingdata$percmissing>52)[,1])
length(drops)
table(drops)
# 56 columns


# Remove columns with missing values(>52%) from dataset
dim(loan_data)
loan_data=loan_data[,!(names(loan_data) %in% drops)]
dim(loan_data)
names(loan_data)
str(loan_data)

drops1 = c("desc","title","url","zip_code","pymnt_plan","policy_code",
           "application_type","verification_status_joint","dti_joint","annual_inc_joint")

test1 = loan_data[,!(names(loan_data) %in% drops1)]
str(test1)
loan_data = test1
dim(loan_data)


library(ggplot2)
options(scipen=999)    
options(max.print=999999) 

###Multivariate----
ggplot(loan_data, aes(loan_data$loan_status,loan_data$loan_amnt)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Loan Amount")+ggtitle("Loan Amount Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$loan_amnt,col = "Skyblue",main="Boxplot For Loan Amount",
        horizontal = TRUE)

###Multivariate----
ggplot(loan_data, aes(loan_data$loan_status,loan_data$funded_amnt)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Funded Amount")+ggtitle("Funded Amount Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$funded_amnt,col = "Skyblue",main="Boxplot For Funded Amount",
        horizontal = TRUE)


###Multivariate----
ggplot(loan_data, aes(loan_data$loan_status,loan_data$funded_amnt_inv)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Fund Amount Investor")+ggtitle("Fund Amount Investor Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$funded_amnt_inv,col = "Skyblue",main="Boxplot For Fund Amount Investor",
        horizontal = TRUE)

###Multivariate----
ggplot(loan_data, aes(loan_data$loan_status,loan_data$int_rate)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Interest Rate")+ggtitle("Interest Rate Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$int_rate,col = "Skyblue",main="Boxplot For Interest Rates",
        horizontal = TRUE)

###Multivariate----
ggplot(loan_data, aes(loan_data$loan_status,loan_data$installment)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Installment Amount")+ggtitle("Installment Amount Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$installment,col = "Skyblue",main="Boxplot For Installment Amount",
        horizontal = TRUE)

###Multivariate----
ggplot(loan_data, aes(loan_data$loan_status,loan_data$annual_inc)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Annual Income")+ggtitle("Annual Income Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$annual_inc,col = "Skyblue",main="Boxplot For Annual Income",
        horizontal = TRUE)

###Multivariate----
ggplot(loan_data, aes(loan_data$loan_status,loan_data$dti)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Debit to Income Ratio")+ggtitle("Debit to Income Ratio Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$dti,col = "Skyblue",main="Boxplot For Debit to Income Ratio",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$delinq_2yrs)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Delinquencey in 2 Yrs")+ggtitle("Delinquencey in 2 Yrs Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$delinq_2yrs,col = "Skyblue",main="Boxplot For Delinquencey in 2 Yrs",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$inq_last_6mths)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Inquiry in last 6 months")+ggtitle("Inquiry in last 6 months Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$inq_last_6mths,col = "Skyblue",main="Boxplot For Inquiry in last 6 months",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$mths_since_last_delinq)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Moths Since Last Deliquency")+ggtitle("Moths Since Last Deliquency Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$mths_since_last_delinq,col = "Skyblue",main="Boxplot For Moths Since Last Deliquency",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$open_acc)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Open Accounts")+ggtitle("Open Accounts Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$open_acc,col = "Skyblue",main="Boxplot For Open Accounts",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$pub_rec)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Derogatory Public Records")+ggtitle("Derogatory Public Records Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$pub_rec,col = "Skyblue",main="Boxplot For Derogatory Public Records",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$revol_bal)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Revolving Balance")+ggtitle("Revolving Balance Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$revol_bal,col = "Skyblue",main="Boxplot For Revolving Balance",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$revol_util)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Revolving Balance Utilization")+ggtitle("Revolving Balance Utilization Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$revol_util,col = "Skyblue",main="Boxplot For Revolving Balance Utilization",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_acc)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Total Accounts")+ggtitle("Total Accounts Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$total_acc,col = "Skyblue",main="Boxplot For Total Accounts",
        horizontal = TRUE)


ggplot(loan_data, aes(loan_data$loan_status,loan_data$out_prncp)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Outstanding Principal")+ggtitle("Outstanding Principal Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$out_prncp,col = "Skyblue",main="Boxplot For Outstanding Principal",
        horizontal = TRUE)


ggplot(loan_data, aes(loan_data$loan_status,loan_data$out_prncp_inv)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Outstanding Principal Inverstor")+ggtitle("Outstanding Principal Inverstor Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$out_prncp_inv,col = "Skyblue",main="Boxplot For Outstanding Principal Inverstor",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_pymnt)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Total Payment")+ggtitle("Total Payment Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$total_pymnt,col = "Skyblue",main="Boxplot For Total Payment",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_pymnt_inv)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Total Payment Inverstor")+ggtitle("Total Payment Inverstor Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$total_pymnt_inv,col = "Skyblue",main="Boxplot For Total Payment Inverstor",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_pymnt_inv)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Total Payment Inverstor")+ggtitle("Total Payment Inverstor Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$total_pymnt_inv,col = "Skyblue",main="Boxplot For Total Payment Inverstor",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_rec_prncp)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Total Received Principal")+ggtitle("Total Received Principal Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  
boxplot(loan_data$total_rec_prncp,col = "Skyblue",main="Boxplot For Total Received Principal",
        horizontal = TRUE)

ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_rec_int)) +
  geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
  ylab("Total Received Interest")+ggtitle("Total Received Interest Vs Loan Status")+xlab("Loan Status")
theme_classic()
######Univariate----  

box<-boxplot(loan_data$total_rec_int,col = "Skyblue",main="Boxplot For Total Received Interest",
        horizontal = TRUE)
x<-which(loan_data$total_rec_int %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)

box<-boxplot(loan_data$total_rec_late_fee,col = "Skyblue",main="Boxplot For Total Received Late Fee",
        horizontal = TRUE)

x<-which(loan_data$total_rec_late_fee %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)

box<-boxplot(loan_data$recoveries,col = "Skyblue",main="Boxplot For Recoveries",
        horizontal = TRUE)

x<-which(loan_data$recoveries %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)

box<-boxplot(loan_data$collection_recovery_fee,col = "Skyblue",main="Boxplot For Collection Recovey Fee",
        horizontal = TRUE)
x<-which(loan_data$collection_recovery_fee %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)

box<-boxplot(loan_data$last_pymnt_amnt,col = "Skyblue",main="Boxplot For Last Payment Amount",
        horizontal = TRUE)
x<-which(loan_data$last_pymnt_amnt %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)

box<-boxplot(loan_data$collections_12_mths_ex_med,col = "Skyblue",main="Boxplot For Collections 12 mnths Excluding medical",
        horizontal = TRUE)

x<-which(loan_data$collections_12_mths_ex_med %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)

box<-boxplot(loan_data$acc_now_delinq,col = "Skyblue",main="Boxplot For Accounts Now Delinquent",
        horizontal = TRUE)
x<-which(loan_data$acc_now_delinq %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)

box<-boxplot(loan_data$tot_coll_amt,col = "Skyblue",main="Boxplot For Total Collection Amount",
        horizontal = TRUE)
x<-which(loan_data$tot_coll_amt %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)

box<-boxplot(loan_data$tot_cur_bal,col = "Skyblue",main="Boxplot For Total Cuurent Balance",
        horizontal = TRUE)
x<-which(loan_data$tot_cur_bal %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)

box<-boxplot(loan_data$total_rev_hi_lim,col = "Skyblue",main="Boxplot For Total revolving high credit/credit limit",
        horizontal = TRUE)

x<-which(loan_data$total_rev_hi_lim %in% box$out) 
table(loan_data$loan_status[x])          
#box$out            
length(box$out)    
table(box$out)
