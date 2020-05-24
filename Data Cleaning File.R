
setwd("E:/r direct/Capstone Project/Project")

Loan1<-read.csv("loan.csv",header=TRUE)

dim(Loan1)
loan_data = Loan1
dim(loan_data)
str(loan_data)

names<-as.data.frame(names(loan_data))
which( colnames(loan_data)=="loan_status" )
#names
#Unique<-as.data.frame(rapply(loan_data,function(x)length(unique(x))))

# Remove Duplicated Records
#nrow(loan_data) - nrow(unique(loan_data))

#Process Missing Columns(with no observations at all)
ncol=rep(nrow(loan_data) ,each=ncol(loan_data))
missingdata=as.data.frame(cbind(colnames=names(loan_data),ncol,
                                nmsg=as.integer(as.character(as.vector(apply(loan_data, 2, function(x) length(which(is.na(x)))))))))

# for each colunm how many obeservations are missing
missingdata$nmsg
missingdata$nmsg=as.numeric(levels(missingdata$nmsg))[missingdata$nmsg]
missingdata$nmsg
missingdata=cbind(missingdata,percmissing=as.integer(missingdata$nmsg/ncol*100))
missingdata$percmissing
drops=as.character(subset(missingdata,missingdata$percmissing>52)[,1])
length(drops)
table(drops)
# 56 columns
#--------------------------------------------------------------------------------------------
# Remove columns with missing values from dataset
dim(loan_data)
loan_data=loan_data[,!(names(loan_data) %in% drops)]
dim(loan_data)
names(loan_data)
str(loan_data)

# --------------
# library(caret)
# nzv <- nearZeroVar(loan_data)
# nzv
# names(loan_data)[nzv]
# 
# View(loan_data)
#------------
drops1 = c("desc","title","url","zip_code","pymnt_plan","policy_code",
           "application_type","verification_status_joint","dti_joint","annual_inc_joint")

test1 = loan_data[,!(names(loan_data) %in% drops1)]
str(test1)
loan_data = test1
dim(loan_data)

########### Do EDA at this point ?? ################

###############   Change factor variables into numeric  ######################

#loan_amnt <- data$loan_amnt
#data$int_rate<-as.numeric(sub("%","",data$int_rate))
#data$revol_util<-as.numeric(sub("%","",data$revol_util))

is.numeric(loan_data$int_rate)
is.numeric(loan_data$revol_util)

loan_data$term<-as.numeric(sub("months","",loan_data$term))
loan_data$term <-ifelse((loan_data$term == 60),1,0)


#install.packages("fastmatch")
library(fastmatch)
loan_data$grade= fmatch(loan_data$grade,c("A","B","C","D","E","F","G"))
str(loan_data$grade)


loan_data$sub_grade= fmatch(loan_data$sub_grade,c("A1","A2","A3","A4","A5",
                                        "B1","B2","B3","B4","B5",
                                        "C1","C2","C3","C4","C5",
                                        "D1","D2","D3","D","D5",
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
home_ownership_dummy = model.matrix(~ home_ownership - 1 ,data=loan_data)
loan_data <- data.frame(loan_data, home_ownership_dummy)
class(home_ownership_dummy)
str(loan_data)
head(home_ownership_dummy)
names(home_ownership_dummy) # null ( check later)
dim(home_ownership_dummy)

#test1 = data %>% filter(home_ownership == "MORTGAGE" | home_ownership == "OWN" | home_ownership == "RENT") only leave three category
#table(test1$home_ownership)


table(loan_data$verification_status)
loan_data$verification_status <- gsub("Source Verified" , "Verified", loan_data$verification_status)
loan_data$verification_status <- ifelse(loan_data$verification_status == "Verified", 1, loan_data$verification_status)
loan_data$verification_status <- ifelse(loan_data$verification_status == "Not Verified", 2, loan_data$verification_status)
loan_data$verification_status <- as.numeric(loan_data$verification_status)

# table(loan_data$verification_status)
# loan_data$verification_status = combineLevels(loan_data$verification_status,levs = c("Source Verified" ,"Verified"), newLabel = c("Source_Verified") )
# loan_data$verification_status <-ifelse((loan_data$verification_status == "Source_Verified"),1,0)
# table(loan_data$verification_status)


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
loan_data$issue_d


loan_data$earliest_cr_line<-paste(loan_data$earliest_cr_line, "-01", sep ="")
loan_data$earliest_cr_line<-parse_date_time(loan_data$earliest_cr_line,"myd")
str(loan_data$earliest_cr_line)
#View(loan_data$earliest_cr_line)

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

# ----------------------------------
# Naveen - need to do
# Response Take "Charged off" and "Default" as default
table(loan_data$loan_status)

loan_data$loan_status=ifelse(loan_data$loan_status=="Late (16-30 days)" 
                    | loan_data$loan_status=="Late (31-120 days)" 
                    | loan_data$loan_status=="Charged Off"  
                    | loan_data$loan_status=="Default"
                    | loan_data$loan_status=="Does not meet the credit policy. Status:Charged Off"
                    ,1,
                    ifelse(loan_data$loan_status=="In Grace Period" 
                         | loan_data$loan_status=="Issued",
                         2,
                    ifelse(loan_data$loan_status=="Current" 
                         |loan_data$loan_status=="Fully Paid"|
                           loan_data$loan_status=="Does not meet the credit policy. Status:Fully Paid"
                         ,3,loan_data$loan_status))) 
                         
table(loan_data$purpose)                       

home_purpose_dummy = model.matrix(~ purpose - 1 ,data=loan_data)
loan_data <- data.frame(loan_data, home_purpose_dummy)


# add code for emp_title , addr_state For these two variables will do any one of stastical test 
#and check for the significance if not signified then will drop these two columns



table(loan_data$addr_state) 

#-----------------------------------

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

str(loan_data[getFactorColumns(loan_data)]) # there r  factor predictors

dim(loan_data[getNumericColumns(loan_data)]) #  numeric predictors

# ---------------------------------------------------------------------------
# tmp2 = apply(loan_data[getNumericColumns(loan_data)],2,function(x){(sd(x))})
# tmp2 = tmp2[tmp2<0.1]
# names(tmp2) 
# tmp2
# 
# library(caret)
# nzv <- nearZeroVar(loan_data)
# nzv
# names(loan_data)[nzv]
# View(home_ownership_dummy)
# 
# # Home_ownership_dummy_OtherNone and home_ownership_dummy_intercept have standard variance less than 0.1,
# #remove these predictors as they have little change
# 
# discard_column = c(names(tmp1),names(tmp2))
# discard_column
# 
# #Discard columnss that have zero variance
# dim(data)
# test2 =data[,!(names(data) %in% discard_column )]
# dim(test2)
# data = test2
# str(data)


#Drop features that have used to geneate new predictors
drop_notuse = c("home_ownership")
test3 = loan_data[,!(names(loan_data) %in% drop_notuse)]
dim(test3)
str(test3)
loan_data = test3
str(loan_data)


# data_factor is the dataframe only include factor data
data_factor = loan_data[getFactorColumns(loan_data)]
str(data_factor)
dim(data_factor)# 4 factor variables


data_num = loan_data[getNumericColumns(loan_data)] #  numeric variables left
str(data_num)
dim(data_num)

# Write cleaned numeric data into Loan_num.csv file
# write.csv(data_num,
#           file="D:\\001UNC Charlotte\\2017Fall\\DSBA6156\\Individual_Project\\Loan_num.csv",row.names = F)
# # Write cleaned factor data into Loan_factor.csv file
# write.csv(data_factor,
#           file="D:\\001UNC Charlotte\\2017Fall\\DSBA6156\\Individual_Project\\Loan_factor.csv",row.names = F)
# # Write cleaned all data into LendingClub_v0.csv fil
# write.csv(data,
#           file="D:\\001UNC Charlotte\\2017Fall\\DSBA6156\\Individual_Project\\LendingClub_v0.csv",row.names = F)

#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
sessionInfo()
gc()
# Imputate missing values in the remaining columns
#missForest
#install.packages("missForest")
library(missForest)

#impute missing values, using all parameters as default values
data_num_imp <- missForest(data)

#check imputed values
head(data_num_imp)


# Drop Columns - Correlated Predictors or Leakage or Predictors that Lead to after-the-fact Prediction
library(caret)
cor1<-as.data.frame(cor(data[getNumericColumns(data)],use = "complete.obs"))
head(cor1)
b[is.na(b)]<-0
corThresh=0.9
CollDrop=findCorrelation(as.matrix(as.data.frame(cor1)),corThresh)
sort(CollDrop)

# Drop and the variables may cause multicolliearity
drops2 = c("average_fico",
           "fico_range_high","fico_range_low","last_fico_range_high","last_fico_range_low","issue_d","earlist_cr_line","last_pymnt_d",
           "next_pymnt_d","last_credit_pull_d","loan_status")


#- -------------------------------------
library(FactoMineR)
cl <- kmeans(data, 1000, iter.max=20)
cah <- HCPC(cl$centers, graph=FALSE, nb.clust=-1)
plot.HCPC(cah, choice="tree")

# ramya ---------------------------



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

