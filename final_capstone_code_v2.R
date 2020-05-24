setwd("E:/r direct/Capstone Project/Project")
##setwd("C:\\Users\\ramya\\Downloads\\lending-club-loan")
Loan1<-read.csv("loan.csv",header=TRUE)

#--------------------------------
#dim(Loan1)
loan_data = Loan1
#dim(loan_data)
#str(loan_data)
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

#--------------------------------------------------------------------------------------

###############   Change factor variables into numeric  ######################

loan_data$term<-as.numeric(sub("months","",loan_data$term))
loan_data$term <-ifelse((loan_data$term == 60),1,0)


#install.packages("fastmatch")
# library(fastmatch)
# loan_data$grade= fmatch(loan_data$grade,c("A","B","C","D","E","F","G"))
# str(loan_data$grade)
# 
# loan_data$sub_grade= fmatch(loan_data$sub_grade,c("A1","A2","A3","A4","A5",
#                                                   "B1","B2","B3","B4","B5",
#                                                   "C1","C2","C3","C4","C5",
#                                                   "D1","D2","D3","D4","D5",
#                                                   "E1", "E2", "E3", "E4", "E5",
#                                                   "F1","F2","F3","F4","F5",
#                                                   "G1","G2","G3","G4","G5"))
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
# Format the date variable and create a new variable to capture credit history
#install.packages("lubridate")
library(lubridate)
loan_data$issue_d<-as.character(loan_data$issue_d)
loan_data$issue_d <- paste(loan_data$issue_d, "-01", sep ="")
Sys.setlocale("LC_TIME", "C") # this command is necessary on my laptop, that depends
loan_data$issue_d <- parse_date_time(loan_data$issue_d,"myd")
loan_data$issue_d


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
##table(loan_data$next_pymnt_d)


table(loan_data$loan_status)
loan_data$loan_status=ifelse(loan_data$loan_status=="Late (16-30 days)" 
                             | loan_data$loan_status=="Late (31-120 days)" 
                             | loan_data$loan_status=="Charged Off"  
                             | loan_data$loan_status=="Default"
                             | loan_data$loan_status=="Does not meet the credit policy. Status:Charged Off"
                             | loan_data$loan_status=="In Grace Period"
                             ,2,ifelse(loan_data$loan_status=="Current" 
                                           |loan_data$loan_status=="Fully Paid"
                                           |loan_data$loan_status=="Does not meet the credit policy. Status:Fully Paid"
                                           |loan_data$loan_status=="Issued"
                                           ,1,loan_data$loan_status)) 
table(loan_data$loan_status)

loan_data$purpose= fmatch(loan_data$purpose,c("car","credit_card","debt_consolidation","educational",
                                              "home_improvement","house", "major_purchase", "medical", "moving","other",
                                              "renewable_energy","small_business","vacation","wedding"))
table(loan_data$purpose)

# add code for emp_title ,addr_state

table(loan_data$delinq_2yrs)

# ----------------------- naveen's univariate box plot analysis -------------------
library(plyr)
options(scipen=999)
options(max.print=999999)

box<- boxplot(loan_data$int_rate,col = "Skyblue",xlab="Boxplot For Interest Rate",horizontal = TRUE)
x<-which(loan_data$int_rate %in% box$out)   # returns the row numbers of outliers
table(loan_data$loan_status[x])            # check the loan status of those outlier records
length(box$out)    # number of outliers
table(box$out)

box<- boxplot(loan_data$installment,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$installment %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)


box<- boxplot(loan_data$inq_last_6mths,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$inq_last_6mths %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<- boxplot(loan_data$annual_inc,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$annual_inc %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

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

# table(loan_data$delinq_2yrs)
# loan_data$delinq_2yrs[x] <-""
# table(loan_data$delinq_2yrs)


box<- boxplot(loan_data$mths_since_last_delinq,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$mths_since_last_delinq %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)


box<- boxplot(loan_data$open_acc,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$open_acc %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)


box<- boxplot(loan_data$installment,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$installment %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<- boxplot(loan_data$revol_bal,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$revol_bal %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)


box<- boxplot(loan_data$revol_util,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$revol_util %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

loan_data$revol_util[x] <-""



box<- boxplot(loan_data$total_acc,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$total_acc %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<- boxplot(loan_data$out_prncp,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$out_prncp %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

loan_data$out_prncp[x] <-""



box<- boxplot(loan_data$out_prncp_inv,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$out_prncp_inv %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)


box<- boxplot(loan_data$total_pymnt,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$total_pymnt %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)


box<- boxplot(loan_data$total_pymnt_inv,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$total_pymnt_inv %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)


box<- boxplot(loan_data$total_rec_prncp,col = "Skyblue",xlab="Boxplot For",horizontal = TRUE)
x<-which(loan_data$total_rec_prncp %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$total_rec_int,col = "Skyblue",main="Boxplot For Total Received Interest",horizontal = TRUE)
x<-which(loan_data$total_rec_int %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$total_rec_late_fee,col = "Skyblue",main="Boxplot For Total Received Late Fee",horizontal = TRUE)
x<-which(loan_data$total_rec_late_fee %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$recoveries,col = "Skyblue",main="Boxplot For Recoveries",horizontal = TRUE)
x<-which(loan_data$recoveries %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$collection_recovery_fee,col = "Skyblue",main="Boxplot For Collection Recovey Fee",horizontal = TRUE)
x<-which(loan_data$collection_recovery_fee %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$last_pymnt_amnt,col = "Skyblue",main="Boxplot For Last Payment Amount",horizontal = TRUE)
x<-which(loan_data$last_pymnt_amnt %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$collections_12_mths_ex_med,col = "Skyblue",main="Boxplot For Collections 12 mnths Excluding medical",horizontal = TRUE)
x<-which(loan_data$collections_12_mths_ex_med %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$acc_now_delinq,col = "Skyblue",main="Boxplot For Accounts Now Delinquent",horizontal = TRUE)
x<-which(loan_data$acc_now_delinq %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$tot_coll_amt,col = "Skyblue",main="Boxplot For Total Collection Amount",horizontal = TRUE)
x<-which(loan_data$tot_coll_amt %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$tot_cur_bal,col = "Skyblue",main="Boxplot For Total Cuurent Balance", horizontal = TRUE)
x<-which(loan_data$tot_cur_bal %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)

box<-boxplot(loan_data$total_rev_hi_lim,col = "Skyblue",main="Boxplot For Total revolving high credit/credit limit",horizontal = TRUE)
x<-which(loan_data$total_rev_hi_lim %in% box$out)
table(loan_data$loan_status[x])
length(box$out)
table(box$out)


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
# data_num1<-data_num
# library(corrplot)
# for(i in 1:ncol(data_num1)){
#   data_num1[,i] <- as.integer(data_num1[,i])
# }
# corrplot(corrplot(cor(data_num1)), na.label = " ")

##----------------------
# drop sub grade , fundedAmt, fundedAmt_inv ,out_prncp_inv,total_pymnt_inv : conclusions from corr plot
drops2 = c("sub_grade" , "funded_amnt", "funded_amnt_inv","out_prncp_inv","total_pymnt_inv")

test2 = loan_data[,!(names(loan_data) %in% drops2)]
str(test2)
loan_data = test2
dim(loan_data)


# dti ,mths_since_last_delinq, revol_util, out_prncp--  outliers -replace with NA's(Code was mentioned against the respective varaibles)   
# id , member_id, emp_title, addr_state - remove the ones not needed for modelling
##drops3 = c("id" , "member_id", "emp_title")
drops3 = c("member_id", "emp_title")

test3 = loan_data[,!(names(loan_data) %in% drops3)]
str(test3)
loan_data = test3
dim(loan_data)

## Removing the Date Columns#####
drop_notuse1 = c("issue_d","earliest_cr_line","last_pymnt_d","next_pymnt_d","last_credit_pull_d")
test31 = loan_data[,!(names(loan_data) %in% drop_notuse1)]
loan_data = test31

loan_data[is.na(loan_data)] <-""

#-----------------------

# Imputate missing values in the remaining columns

# check which columns have NAs
colnames(loan_data)[colSums(is.na(loan_data)) > 0]

varnums<- function(x) {w=as.data.frame(c(1:length(colnames(x))),
                                       paste0('# ',colnames(x)))
names(w)= c("# Variable_name/Index")
w}
varnums(loan_data)

str(loan_data)

colnames(loan_data)

#---
# mice 
# install.packages("mice")
library(mice)
md.pattern(loan_data)
mice_imputed_Data <- mice(loan_data, m=5, maxit = 5, method = 'pmm', seed = 500)
summary(mice_imputed_Data)
write.csv(loan_data, file = "Post Imputation.csv")

####Reading the data from Post Imputation ########
post_imputation_data<-read.csv("Post Imputation.csv",header=TRUE)

