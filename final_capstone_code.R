setwd("E:/r direct/Capstone Project/Project")
Loan1<-read.csv("loan.csv",header=TRUE)

#--------------------------------
dim(Loan1)
loan_data = Loan1
dim(loan_data)
str(loan_data)
names<-as.data.frame(names(loan_data))

#-------------------------------------------

# check missing values for all the vars

#install.packages("naniar")
# library(naniar)
# gg_miss_var(Loan1,show_pct=TRUE)


#Process Missing Columns(with no observations at all)
ncol=rep(nrow(loan_data) ,each=ncol(loan_data))
missingdata=as.data.frame(cbind(colnames=names(loan_data),ncol,
                                nmsg=as.integer(as.character(as.vector(apply(loan_data, 2, function(x) length(which(is.na(x)))))))))

# for each colunm how many obeservations are missing
missingdata$nmsg
# above commmand is not showing emp_length, earliest_cr_line,last_pymt_d,next_pymt_d,last_credit_pull_d,verification_status_joint
# as columns with NAs though they have NAs in the Data ...why??

missingdata$nmsg=as.numeric(levels(missingdata$nmsg))[missingdata$nmsg]
missingdata$nmsg
missingdata=cbind(missingdata,percmissing=as.integer(missingdata$nmsg/ncol*100))
missingdata$percmissing
drops=as.character(subset(missingdata,missingdata$percmissing>52)[,1])
length(drops)
table(drops)
# 56 columns

# ------------------ EDA start-----------------------------------------#

# library(ggplot2)
# options(scipen=999)  
# options(max.print=999999) 
# 
# # Asha variables eda code (ramya)
# ggplot(data=Loan1, aes(loan_status,all_util))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("all_util")+ggtitle("Balance to credit limit on all trades Vs Loan status")+xlab("loan_status")
# 
# hist(Loan1$all_util ,main="Balance to credit limit(on all trades) distribution" , xlab="all_util",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status,annual_inc_joint))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("annual_inc_joint")+ggtitle("combined annual income provided by co-borrowers Vs Loan status")+xlab("loan_status")
# 
# hist(Loan1$annual_inc_joint ,main="Combined annual income provided by co-borrowers distribution" , xlab="annual_inc_joint",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status,dti_joint))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("dti_joint")+ggtitle("Debt-to-income ratio of co-borrower Vs Loan status")+xlab("loan_status")
# 
# hist(Loan1$dti_joint ,main="Debt-to-income ratio of co-borrower distribution" , xlab="dti_joint",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status,il_util ))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("il_util ")+ggtitle("Ratio of total current balance to credit limit on all accounts Vs Loan status")+xlab("loan_status")
# 
# hist(Loan1$il_util  ,main="Ratio of total current balance to credit limit on all accounts distribution" , xlab="il_util ",col="cyan")
# 
# 
# # nabasish var eda  code
# # devlina var eda  code
# 
# 
# # ramya's vars - bivariate and univariate 
# ggplot(data=Loan1, aes(loan_status,revol_bal))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("revolving balance")+ggtitle("Revolving balance Vs Loan status")+xlab("loan status")
# 
# hist(Loan1$revol_bal ,main="Revolving balance distribution" , xlab="Revolving balance", xlim=c(0,300000),col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status,initial_list_status))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("initial_list_status")+ggtitle("Intial list status Vs Loan status")+xlab("loan_status")
# 
# barplot(table(Loan1$initial_list_status) ,main="Initial list status distribution" , xlab="initial list status of loan",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status,out_prncp))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("out_prncp")+ggtitle("Outstanding principal for total amount funded Vs Loan status")+xlab("loan_status")
# 
# hist(Loan1$out_prncp ,main="Outstanding principle distribution" , xlab="Outstanding principle",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status,out_prncp_inv))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("out_prncp_inv")+ggtitle("Outstanding principal for portion of total amount funded by investors Vs Loan status")+xlab("loan_status")
# 
# hist(Loan1$out_prncp_inv ,main="Outstanding principle(for portion of total amount funded by investors) distribution" , xlab="out_prncp_inv",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, total_pymnt))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("total_pymnt ")+ggtitle("Payments received to date  Vs  Loan status")+xlab("loan_status")
# 
# hist(Loan1$total_pymnt ,main="Total payment received to date distribution" , xlab="total payment",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, total_pymnt_inv))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("total_pymnt_inv ")+ggtitle("Payments received to date for portion of total amount funded by investors  Vs  Loan status")+xlab("loan_status")
# 
# hist(Loan1$total_pymnt_inv ,main="Total payment received to date(for portion of amount funded by investors) distribution" , xlab="total_pymnt_inv",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, total_rec_prncp))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("total_rec_prncp ")+ggtitle("Received principle to date  Vs  Loan status")+xlab("loan_status")
# 
# hist(Loan1$total_rec_prncp ,main="Total received principle to date distribution" , xlab="total_rec_prncp",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, total_rec_int))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("total_rec_int")+ggtitle("Interest received to date  Vs  Loan status")+xlab("loan_status")
# 
# hist(Loan1$total_rec_int ,main="Total received interest to date distribution" , xlab="total_rec_int",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, total_rec_late_fee))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("total_rec_late_fee ")+ggtitle("Late fees received to data  Vs  Loan status")+xlab("loan_status")
# 
# hist(Loan1$total_rec_late_fee ,main="Total received late fees distribution" , xlab="total_rec_late_fee",xlim=c(0,100),col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, recoveries))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("recoveries ")+ggtitle("post charge-off gross recovery  Vs  Loan status")+xlab("loan_status")
# 
# hist(Loan1$recoveries ,main="Post charge-off gross recovery distribution" , xlab="recoveries",xlim=c(0,10000),col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, collection_recovery_fee))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("collection_recovery_fee ")+ggtitle("post charge-off collection fee  Vs Loan status")+xlab("loan_status")
# 
# hist(Loan1$collection_recovery_fee ,main="Post charge-off collection recovery fee distribution" , xlab="collection_recovery_fee",xlim=c(0,3000),col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, last_pymnt_amnt))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("last_pymnt_amnt ")+ggtitle("Last total payment amount received Vs Loan status")+xlab("loan_status")
# 
# hist(Loan1$last_pymnt_amnt ,main="Last total payment amount received distribution" , xlab="last_pymnt_amnt",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, policy_code))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("policy_code ")+ggtitle("Policy code  Vs Loan status")+xlab("loan_status")
# 
# barplot(table(Loan1$policy_code) ,main="Policy code distribution" , xlab="policy_code",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, application_type))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("application_type ")+ggtitle(" Application type Vs Loan status")+xlab("loan_status")
# 
# barplot(table(Loan1$application_type) ,main="Loan application type distribution" , xlab="application_type",col="cyan")
# 
# 
# ggplot(data=Loan1, aes(loan_status, verification_status_joint))+geom_bar(stat="identity",aes(fill=loan_status))+coord_flip()+xlab("")+
#   ylab("verification_status_joint ")+ggtitle("Income verification status of co-borrower  Vs Loan status")+xlab("loan_status")
# 
# barplot(table(Loan1$verification_status_joint) ,main="Income verification status of co-borrower distribution" , xlab="verification_status_joint",col="cyan")
# 
 
# Remove columns with missing values(>52%) from dataset
dim(loan_data)
loan_data=loan_data[,!(names(loan_data) %in% drops)]
dim(loan_data)
names(loan_data)
str(loan_data)


# -------------------- check the variance of all the variables ---------------------------
# library(caret)
# nzv <- nearZeroVar(loan_data)
# nzv
# names(loan_data)[nzv]

#------------------ dropping variables which are not required for model -----------------------

drops1 = c("desc","title","url","zip_code","pymnt_plan","policy_code",
           "application_type","verification_status_joint","dti_joint","annual_inc_joint")

test1 = loan_data[,!(names(loan_data) %in% drops1)]
str(test1)
loan_data = test1
dim(loan_data)


#-----------naveen's  bi-variate box plot analysis eda code ----------------

# library(ggplot2)
# options(scipen=999)    
# options(max.print=999999) 
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$loan_amnt)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Loan Amount")+ggtitle("Loan Amount Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$funded_amnt)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Funded Amount")+ggtitle("Funded Amount Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$funded_amnt_inv)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Fund Amount Investor")+ggtitle("Fund Amount Investor Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$int_rate)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Interest Rate")+ggtitle("Interest Rate Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$installment)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Installment Amount")+ggtitle("Installment Amount Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$annual_inc)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Annual Income")+ggtitle("Annual Income Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$dti)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Debit to Income Ratio")+ggtitle("Debit to Income Ratio Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$delinq_2yrs)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Delinquencey in 2 Yrs")+ggtitle("Delinquencey in 2 Yrs Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$inq_last_6mths)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Inquiry in last 6 months")+ggtitle("Inquiry in last 6 months Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$mths_since_last_delinq)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Months Since Last Deliquency")+ggtitle("Months Since Last Deliquency Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$open_acc)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Open Accounts")+ggtitle("Open Accounts Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$pub_rec)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Derogatory Public Records")+ggtitle("Derogatory Public Records Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$revol_bal)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Revolving Balance")+ggtitle("Revolving Balance Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$revol_util)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Revolving Balance Utilization")+ggtitle("Revolving Balance Utilization Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_acc)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Total Accounts")+ggtitle("Total Accounts Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$out_prncp)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Outstanding Principal")+ggtitle("Outstanding Principal Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$out_prncp_inv)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Outstanding Principal Inverstor")+ggtitle("Outstanding Principal Inverstor Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_pymnt)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Total Payment")+ggtitle("Total Payment Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_pymnt_inv)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Total Payment Inverstor")+ggtitle("Total Payment Inverstor Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_rec_prncp)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Total Received Principal")+ggtitle("Total Received Principal Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_rec_int)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Total Received Interest")+ggtitle("Total Received Interest Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_rec_late_fee)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Total Received Late Fee")+ggtitle("Total Received Late Fee Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$recoveries)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Recoveries")+ggtitle("Recoveries Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$collection_recovery_fee)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Collection Recovey Fee")+ggtitle("Collection Recovey Fee Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$last_pymnt_amnt)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Last Payment Amount")+ggtitle("Last Payment Amount Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$collections_12_mths_ex_med)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Collections 12 mnths Excluding medical ")+ggtitle("Collections 12 mnths Excluding medical Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$acc_now_delinq)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Accounts Now Delinquent")+ggtitle("Accounts Now Delinquent Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$tot_coll_amt)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Total Collection Amount")+ggtitle("Total Collection Amount Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$tot_cur_bal)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Total Cuurent Balance")+ggtitle("Total Cuurent Balance Vs Loan Status")+xlab("Loan Status")
# theme_classic()
# 
# ggplot(loan_data, aes(loan_data$loan_status,loan_data$total_rev_hi_lim)) +
#   geom_boxplot(aes(fill = loan_data$loan_status))+coord_flip()+xlab("")+
#   ylab("Total revolving high credit/credit limit")+ggtitle("Total revolving high credit/credit limit Vs Loan Status")+xlab("Loan Status")
# theme_classic()

###############   Change factor variables into numeric  ######################

loan_data$term<-as.numeric(sub("months","",loan_data$term))
loan_data$term <-ifelse((loan_data$term == 60),1,0)


#install.packages("fastmatch")
library(fastmatch)
loan_data$grade= fmatch(loan_data$grade,c("A","B","C","D","E","F","G"))
str(loan_data$grade)


loan_data$sub_grade= fmatch(loan_data$sub_grade,c("A1","A2","A3","A4","A5",
                                                  "B1","B2","B3","B4","B5",
                                                  "C1","C2","C3","C4","C5",
                                                  "D1","D2","D3","D4","D5",
                                                  "E1", "E2", "E3", "E4", "E5",
                                                  "F1","F2","F3","F4","F5",
                                                  "G1","G2","G3","G4","G5"))
str(loan_data$sub_grade)


table(loan_data$emp_length)
loan_data$emp_length <- gsub(" years" , "", loan_data$emp_length)
loan_data$emp_length <- gsub(" year" , "", loan_data$emp_length)
loan_data$emp_length <- ifelse(loan_data$emp_length == "10+", 10, loan_data$emp_length)
loan_data$emp_length <- ifelse(loan_data$emp_length == "< 1", 0, loan_data$emp_length)
loan_data$emp_length <- as.numeric(loan_data$emp_length)
table(loan_data$emp_length)


table(loan_data$home_ownership)
#install.packages("rockchalk")
library(rockchalk)
loan_data$home_ownership = combineLevels(loan_data$home_ownership,levs = c("NONE", "OTHER" ,"ANY"), newLabel = c("OtherNone") )
table(loan_data$home_ownership)
loan_data$home_ownership= fmatch(loan_data$home_ownership,c("MORTGAGE","OWN","RENT","OtherNone"))
head(loan_data$home_ownership)


table(loan_data$verification_status)
loan_data$verification_status = combineLevels(loan_data$verification_status,levs = c("Source Verified" ,"Verified"), newLabel = c("Source_Verified") )
loan_data$verification_status <-ifelse((loan_data$verification_status == "Source_Verified"),1,0)
table(loan_data$verification_status)


table(loan_data$initial_list_status)
loan_data$initial_list_status <-ifelse((loan_data$initial_list_status == "w"),1,0) # whole=1, fractional = 0
table(loan_data$initial_list_status)
class(loan_data$initial_list_status)

#--------------------------------------------------------------------------------#
# Format the date variable and creat a new variable to capture credit history
#install.packages("lubridate")
library(lubridate)
loan_data$issue_d<-as.character(loan_data$issue_d)
loan_data$issue_d <- paste(loan_data$issue_d, "-01", sep ="")
Sys.setlocale("LC_TIME", "C") # this command is necessary on my laptop, that depends
loan_data$issue_d <- parse_date_time(loan_data$issue_d,"myd")
##loan_data$issue_d


loan_data$earliest_cr_line<-paste(loan_data$earliest_cr_line, "-01", sep ="")
loan_data$earliest_cr_line<-parse_date_time(loan_data$earliest_cr_line,"myd")
str(loan_data$earliest_cr_line)


time_since_first_credit<-loan_data$issue_d - loan_data$earliest_cr_line
loan_data$time_since_first_credit<-as.numeric(time_since_first_credit)
length(time_since_first_credit)
str(loan_data$time_since_first_credit)


loan_data$last_pymnt_d<-paste(loan_data$last_pymnt_d, "-01", sep ="")
loan_data$last_pymnt_d<-parse_date_time(loan_data$last_pymnt_d,"myd")

loan_data$last_credit_pull_d<-paste(loan_data$last_credit_pull_d, "-01", sep ="")
loan_data$last_credit_pull_d<-parse_date_time(loan_data$last_credit_pull_d,"myd")


loan_data$next_pymnt_d<-paste(loan_data$next_pymnt_d, "-01", sep ="")
loan_data$next_pymnt_d<-parse_date_time(loan_data$next_pymnt_d,"myd")
table(loan_data$next_pymnt_d)


table(loan_data$loan_status)
loan_data$loan_status=ifelse(loan_data$loan_status=="Late (16-30 days)" 
                             | loan_data$loan_status=="Late (31-120 days)" 
                             | loan_data$loan_status=="Charged Off"  
                             | loan_data$loan_status=="Default"
                             | loan_data$loan_status=="Does not meet the credit policy. Status:Charged Off"
                             ,3,
                             ifelse(loan_data$loan_status=="In Grace Period" 
                                    | loan_data$loan_status=="Issued",
                                    2,
                                    ifelse(loan_data$loan_status=="Current" 
                                           |loan_data$loan_status=="Fully Paid"|
                                             loan_data$loan_status=="Does not meet the credit policy. Status:Fully Paid"
                                           ,1,loan_data$loan_status))) 


loan_data$purpose= fmatch(loan_data$purpose,c("car","credit_card","debt_consolidation","educational",
                                              "home_improvement","house", "major_purchase", "medical", "moving","other",
                                              "renewable_energy","small_business","vacation","wedding"))
table(loan_data$purpose)

# add code for emp_title ,addr_state


# ----------------------- naveen's univariate box plot analysis -------------------
# library(plyr)
# options(scipen=999)
# options(max.print=999999)
#
# box<- boxplot(loan_data$int_rate,col = "Skyblue",xlab="Boxplot For Interest Rate",horizontal = TRUE)
# x<-which(loan_data$int_rate %in% box$out)   # returns the row numbers of outliers
# table(loan_data$loan_status[x])            # check the loan status of those outlier records
# length(box$out)    # number of outliers
# table(box$out)
#
# box<- boxplot(loan_data$installment,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$installment %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
#
# box<- boxplot(loan_data$inq_last_6mths,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$inq_last_6mths %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<- boxplot(loan_data$annual_inc,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$annual_inc %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
box<- boxplot(loan_data$dti,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$dti %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

loan_data$dti[x] <-""

loan_data1<-loan_data
newdata <- loan_data1[ which(loan_data1$id==66415476), ]
View(newdata)


box<- boxplot(loan_data$delinq_2yrs,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$delinq_2yrs %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

loan_data$delinq_2yrs[x] <-""



# box<- boxplot(loan_data$mths_since_last_delinq,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$mths_since_last_delinq %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
#
# box<- boxplot(loan_data$open_acc,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$open_acc %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
#
# box<- boxplot(loan_data$installment,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$installment %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<- boxplot(loan_data$revol_bal,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$revol_bal %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#

box<- boxplot(loan_data$revol_util,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$revol_util %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

loan_data$revol_util[x] <-""



# box<- boxplot(loan_data$total_acc,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$total_acc %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)

box<- boxplot(loan_data$out_prncp,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$out_prncp %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

loan_data$out_prncp[x] <-""



# box<- boxplot(loan_data$out_prncp_inv,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$out_prncp_inv %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
#
# box<- boxplot(loan_data$total_pymnt,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$total_pymnt %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
#
# box<- boxplot(loan_data$total_pymnt_inv,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$total_pymnt_inv %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
#
# box<- boxplot(loan_data$total_rec_prncp,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
# x<-which(loan_data$total_rec_prncp %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$total_rec_int,col = "Skyblue",main="Boxplot For Total Received Interest",horizontal = TRUE)
# x<-which(loan_data$total_rec_int %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$total_rec_late_fee,col = "Skyblue",main="Boxplot For Total Received Late Fee",horizontal = TRUE)
# x<-which(loan_data$total_rec_late_fee %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$recoveries,col = "Skyblue",main="Boxplot For Recoveries",horizontal = TRUE)
# x<-which(loan_data$recoveries %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$collection_recovery_fee,col = "Skyblue",main="Boxplot For Collection Recovey Fee",horizontal = TRUE)
# x<-which(loan_data$collection_recovery_fee %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$last_pymnt_amnt,col = "Skyblue",main="Boxplot For Last Payment Amount",horizontal = TRUE)
# x<-which(loan_data$last_pymnt_amnt %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$collections_12_mths_ex_med,col = "Skyblue",main="Boxplot For Collections 12 mnths Excluding medical",horizontal = TRUE)
# x<-which(loan_data$collections_12_mths_ex_med %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$acc_now_delinq,col = "Skyblue",main="Boxplot For Accounts Now Delinquent",horizontal = TRUE)
# x<-which(loan_data$acc_now_delinq %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$tot_coll_amt,col = "Skyblue",main="Boxplot For Total Collection Amount",horizontal = TRUE)
# x<-which(loan_data$tot_coll_amt %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$tot_cur_bal,col = "Skyblue",main="Boxplot For Total Cuurent Balance", horizontal = TRUE)
# x<-which(loan_data$tot_cur_bal %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)
#
# box<-boxplot(loan_data$total_rev_hi_lim,col = "Skyblue",main="Boxplot For Total revolving high credit/credit limit",horizontal = TRUE)
# x<-which(loan_data$total_rev_hi_lim %in% box$out)
# table(loan_data$loan_status[x])
# length(box$out)
# table(box$out)

#-----------------------------------------
#Check and output the datatype
getNumericColumns<-function(t){
  tn = sapply(t,function(x){is.numeric(x)})
  return(names(tn)[which(tn)])
}

getCharColumns<-function(t){
  tn = sapply(t,function(x){is.character(x)})
  return(names(tn)[which(tn)])
}

getFactorColumns<-function(t){
  tn = sapply(t,function(x){is.factor(x)})
  return(names(tn)[which(tn)])
}

getIndexsOfColumns <- function(t,column_names){
  return(match(column_names,colnames(t)))
}

tmp1 = apply(loan_data[getCharColumns(loan_data)],2,function(x){length(unique(x))})
tmp1 = tmp1[tmp1==1]
str(loan_data[getCharColumns(loan_data)])# No character features left
str(loan_data)

#length(table(Loan1$addr_state))

str(loan_data[getFactorColumns(loan_data)]) # factor predictors

dim(loan_data[getNumericColumns(loan_data)]) #  numeric predictors


# data_factor is the dataframe only include factor data
data_factor = loan_data[getFactorColumns(loan_data)]
str(data_factor)
dim(data_factor)# factor variables


data_num = loan_data[getNumericColumns(loan_data)] #  numeric variables left
str(data_num)
dim(data_num)
class(data_num)

###--------- multivariate analysis corr plot -----------####
data_num1<-data_num
library(corrplot)
for(i in 1:ncol(data_num1)){
  data_num1[,i] <- as.integer(data_num1[,i])
}
corrplot(corrplot(cor(data_num1)), na.label = " ")

##----------------------
## From the Carrplot we have identified that below variables are higly correlated

###loan Amount<---fund amt, funamt investor

cor(loan_data$out_prncp,loan_data$out_prncp_inv)
res <- cor.test(loan_data$out_prncp, loan_data$out_prncp_inv, 
                method = "pearson")
res

drops2 = c("sub_grade" , "funded_amnt", "funded_amnt_inv","out_prncp_inv",
           "emp_title",
           "addr_state")

test2 = loan_data[,!(names(loan_data) %in% drops2)]
str(test2)
loan_data = test2
dim(loan_data)

#### Outlier IMputation Done Above########
###dti ,mths_since_last_delinq, revol_util, out_prncp
## Removing the Date Columns#####

drop_notuse1 = c("issue_d","earliest_cr_line","last_pymnt_d","next_pymnt_d","last_credit_pull_d")
test31 = loan_data[,!(names(loan_data) %in% drop_notuse1)]
loan_data = test31

loan_data[is.na(loan_data)] <-""

write.csv(loan_data, file = "LoanImputation.csv")



#-----------------------

# Imputate missing values in the remaining columns

# check which columns have NAs
colnames(loan_data)[colSums(is.na(loan_data)) > 0]

varnums<- function(x) {w=as.data.frame(c(1:length(colnames(x))),
                                       paste0('# ',colnames(x)))
names(w)= c("# Variable_name/Index")
w}
varnums(loan_data)

#---
# mice 
# install.packages("mice")
library(mice)




#-----------------------

# Drop Columns - Correlated Predictors or Leakage or Predictors that Lead to after-the-fact Prediction
library(caret)
cor1<-as.data.frame(cor(data[getNumericColumns(data)],use = "complete.obs"))
head(cor1)
b[is.na(b)]<-0
corThresh=0.9
CollDrop=findCorrelation(as.matrix(as.data.frame(cor1)),corThresh)
sort(CollDrop)

# Drop and the variables may cause multicolliearity
drops2 = c("issue_d","earlist_cr_line","last_pymnt_d","next_pymnt_d","last_credit_pull_d","loan_status")

#- -------------------------------------
library(FactoMineR)
cl <- kmeans(data, 1000, iter.max=20)
cah <- HCPC(cl$centers, graph=FALSE, nb.clust=-1)
plot.HCPC(cah, choice="tree")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Descriptive analysis
library(DescTools)
Desc(data1$default,main="Default Status")
as.matrix(prop.table(table(data1$default,data1$grade),2)*100)


library(dplyr)
install.packages("stringr")
library(stringr)
library(ggplot2)
tmp = data %>% group_by(default) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(data)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=default,y=ncount,fill=default)) + geom_bar(stat="identity") +
  geom_text(aes(label=ncount_p),vjust = 2)



plot(data$default,data$loan_amnt,xlab="Loan Status",ylab="Loan Amount")

#------------------------------------------------------------------------
#Look at all numeric features
str(data1)
str(data1[getNumericColumns(data1)])
dim(data1[getNumericColumns(data1)])

data1_num = data1[getNumericColumns(data1)]
str(data1_num)
data1_num$default

#Scaled and center data


trans_model = preProcess(data1_num,method = c("center","scale"))
data2 = predict(trans_model,data1_num)


model_logistic = glm(default~., family = "binomial",maxit = 100,data=data1_num)
summary(model_logistic)
anova(model_logistic)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Split the dataset into training and test set
nrow(data1_num)
train_smp_size<-floor(0.75*nrow(data1_num))
set.seed(123)
train<-sample(seq_len(nrow(data1_num)),size=train_smp_size)
length(train)
data1_num_train<-data1_num[train,]
data1_num_test<-data1_num[-train,]
dim(data1_num_train)
dim(data1_num_test)
length(data1_num_train$default)

y_train = as.factor(data1_num_train$default)
(y_train)
X_train = as.data.frame(subset(data1_num_train,select = -default))
y_test = data1_num_test$default
X_test = as.data.frame(subset(data1_num_test,selet = -default))
head(X_train)
length(y_train)
dim(X_train)

#-----------------------------------------------------------------------------------
library(caret)
library(gbm)
library(plyr)
install.packages('e1071', dependencies=TRUE)
library(e1071)
library(dplyr)
library(bnclassify)


train_control <- trainControl(method = "cv", number = 10)
mod <- train(default~., data=data1_num_train, method = "svmLinear", trControl = train_control, na.action=na.exclude)




train_control <- trainControl(method = "cv", number = 10)
grid <- expand.grid(smooth = 2)
nb <- train(y_train ~ as.factor(loan_amnt),
            data=data1_num_train, trControl=train_control, method="nbDiscrete", tuneGrid=grid,na.action=na.exclude)


warnings()



loan_amnt+term+int_rate+installment+grade+emp_length+annual_inc+dti+delinq_2yrs+open_acc+
  pub_rec+revol_util+time_since_first_credit+average_fico_last

grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE),smooth = 2)


glmGrid = expand.grid(.parameter = seq(1, 10, 1))
glmCV <- train(y_train ~ , data = data1_num_train, method = 'glm', trControl = train_control, 
               tuneGrid = data.frame(glmGrid))

predictCV = predict(glmCV, newdata = test, type = "prob")





gbm.fit<- train( y=as.factor(y_train), x=X_train, 
                 method = "gbm", 
                 trControl = fitControl,verbose = FALSE)

# Output cleaed data in to csv file
write.csv(Loan1,
          file="D:\\001UNC Charlotte\\2017Fall\\DSBA6156\\Individual_Project\\Loan1.csv",
          row.names=F,quote=F)




# ----------To make clear of datatype--------------#
datatype = unlist(lapply(data1,class))
table(datatype)
names(datatype)
datatype_character = names(datatype)[class =="character"]
datatype_character
datatype_factor = names(datatype)[class == "factor"]
datatype_factor
datatype_numeric = names(datatype)[class =="numeric"]
datatype_numeric
datatype_int = names(datatype)[class =="integer"]
datatype_int



class_character = setdiff(datatype_character,c("title"))
class_factor = setdiff(datatype_factor,c("loan_status"))
class_numeric = setdiff(datatype_numeric,c("member_id","default","addr_state","initial_list_status"))
class_numeric
class_int = setdiff(datatype_int, c("loan_amnt","initial_list_status"))
class_int
data_character = data1[,names(data1) %in% class_character]
data_factor = data1[,names(data1) %in% class_factor]
data_numeric = data1[,names(data1) %in% class_numeric]
str(data_numeric)
data_int = data1[,names(data1) %in% class_int]
str(data_int)
data1 = data.frame("title",data_character,
                   "loan_status","addr_state","initial_list_status","application_type",data_factor,
                   "member_id","default",data_numeric,
                   "loan_amnt",data_int)
dim(data1)
head(data1)

