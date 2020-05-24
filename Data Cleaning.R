####Include text analysis
library(caret)
##install.packages("fastmatch")

setwd("E:/r direct/Capstone Project/Project")
library(readr)
Loan <- read.csv("loan.csv",header=TRUE)

dim(Loan)
data = Loan
dim(data)
str(data)

names<-as.data.frame(names(data))
names
##Unique<-as.data.frame(rapply(data,function(x)length(unique(x))))

# Remove Duplicated Records

##nrow(data) - nrow(unique(data))

ncol=rep(nrow(data) ,each=ncol(data))
missingdata=as.data.frame(cbind(colnames=names(data),ncol,
                                nmsg=as.integer(as.character(as.vector(apply(data, 2, function(x) length(which(is.na(x)))))))))
# for each colunm how many obeservations are missing
missingdata$nmsg
missingdata$nmsg=as.numeric(levels(missingdata$nmsg))[missingdata$nmsg]
missingdata$nmsg
missingdata=cbind(missingdata,percmissing=as.integer(missingdata$nmsg/ncol*100))
missingdata$percmissing
drops=as.character(subset(missingdata,missingdata$percmissing>52)[,1])
length(drops)
table(drops)
#--------------------------------------------------------------------------------------------
# Remove columns with missing values from dataset
dim(data)
data=data[,!(names(data) %in% drops)]
dim(data)
names(data)
str(data)

#checking variance
#nzv <- nearZeroVar(data)
##18 31 41 42 43 47 48 49 50 51 52 53
#names(data)[nzv]


##pymnt_plan
##policy_code
##application_type
###verification_status_joint
##annual_inc_joint
##dti_joint

View(data)

names(data)[nzv]

drops1 = c("desc","url","zip_code","application_type","title",
           "pymnt_plan","policy_code","verification_status_joint","annual_inc_joint","dti_joint")

test1 = data[,!(names(data) %in% drops1)]
str(test1)
data = test1
dim(data)

# Change factor variables into numeric
###loan_amnt<-data$loan_amnt
term<-as.numeric(sub("months","",data$term))
###term <-ifelse((term == 60),1,0)
data$term <-term
table(data$term)
data$int_rate<-as.numeric(sub("%","",data$int_rate))
data$revol_util<-as.numeric(sub("%","",data$revol_util))
library(fastmatch)
data$grade= fmatch(data$grade,c("A","B","C","D","E","F","G"))
str(data$grade)
data$sub_grade= fmatch(data$sub_grade,c("A1","A2","A3","A4","A5",
                                        "B1","B2","B3","B4","B5",
                                        "C1","C2","C3","C4","C5",
                                        "D1","D2","D3","D","D5",
                                        "E1", "E2", "E3", "E4", "E5",
                                        "F1","F2","F3","F4","F5",
                                        "G1","G2","G3","G4","G5"))
str(data$sub_grade)

data$emp_length <- gsub(" years" , "", data$emp_length)
data$emp_length <- gsub(" year" , "", data$emp_length)
data$emp_length <- ifelse(data$emp_length == "10+", 10, data$emp_length)
data$emp_length <- ifelse(data$emp_length == "< 1", 0, data$emp_length)
data$emp_length <- as.numeric(data$emp_length)

table(data$emp_length)
str(data$emp_length)
# emp_length<-data$emp_length
# emp_length = fmatch(data$emp_length,
#                     c("1 year","2 years",
#                       " 3 years","4 years","5 years","6 years",
#                       "7 years","8 years",
#                       "9 years","10+ years"
#                     ))
# table(emp_length)
# 
# 
# str(emp_length)
# table(emp_length)
# 
# data$emp_length<-emp_length
# 
# table(data$emp_length)

table(data$home_ownership)
##install.packages("rockchalk")
library(rockchalk)
home_ownership = data$home_ownership
home_ownership1 = combineLevels(home_ownership,levs = c("NONE", "OTHER","ANY"), newLabel = c("OtherNone") )
table(home_ownership1)
str(home_ownership1)
# -------------------change factor variable into dummy --------------------------------#

home_ownership_dummy =as.factor(home_ownership1)
home_ownership_dummy
home_ownership_dummy = model.matrix(~home_ownership_dummy)
class(home_ownership_dummy)
home_ownership_dummy = as.data.frame(home_ownership_dummy)
head(home_ownership_dummy)
names(home_ownership_dummy)
dim(home_ownership_dummy)## need to check for interesept column name
data = cbind.data.frame(data,home_ownership_dummy)
str(data)
#test1 = data %>% filter(home_ownership == "MORTGAGE" | home_ownership == "OWN" | home_ownership == "RENT") only leave three category
#table(test1$home_ownership)
---------------------------------------------------------------------------------------
table(data$verification_status)
data$verification_status <- gsub("Source Verified" , "Verified", data$verification_status)
data$verification_status <- ifelse(data$verification_status == "Verified", 1, data$verification_status)
data$verification_status <- ifelse(data$verification_status == "Not Verified", 2, data$verification_status)
data$verification_status <- as.numeric(data$verification_status)

table(data$initial_list_status)
initial_list_status <-ifelse((data$initial_list_status == "w"),1,0) # whole=1, fractional = 0
class(initial_list_status)
data$initial_list_status <-initial_list_status




#--------------------------------------------------------------------------------#
# Formate the date variable and creat a new variable to capture credit history
install.packages("lubridate")
library(lubridate)
data$issue_d<-as.character(data$issue_d)
data$issue_d <- paste(data$issue_d, "-01", sep ="")
Sys.setlocale("LC_TIME", "C") # this command is necessary on my laptop, that depends
data$issue_d <- parse_date_time(data$issue_d,"myd")
data$issue_d

earliest_cr_line<-data$earliest_cr_line
earliest_cr_line<-as.character(earliest_cr_line)
earliest_cr_line<-paste(earliest_cr_line, "-01", sep ="")
data$earliest_cr_line<-parse_date_time(earliest_cr_line,"myd")
str(data$earliest_cr_line)

time_since_first_credit<-data$issue_d - data$earliest_cr_line
data$time_since_first_credit<-as.numeric(time_since_first_credit)
length(time_since_first_credit)
str(data$time_since_first_credit)


last_pymnt_d<-data$last_pymnt_d
last_pymnt_d<-as.character(last_pymnt_d)
last_pymnt_d<-paste(last_pymnt_d, "-01", sep ="")
data$last_pymnt_d<-parse_date_time(last_pymnt_d,"myd")


last_credit_pull_d<-data$last_credit_pull_d
last_credit_pull_d<-as.character(last_credit_pull_d)
last_credit_pull_d<-paste(last_credit_pull_d, "-01", sep ="")
data$last_credit_pull_d<-parse_date_time(last_credit_pull_d,"myd")

next_pymnt_d<-data$next_pymnt_d
next_pymnt_d<-as.character(next_pymnt_d)
next_pymnt_d<-paste(next_pymnt_d, "-01", sep ="")
data$next_pymnt_d<-parse_date_time(next_pymnt_d,"myd")
table(data$next_pymnt_d)

# Remove xxx from zip code
#data$zip_code<-strtrim(data$zip_code,3)

# Creat New variables to measure the average FICO credit score
data$average_fico_last<-(data$last_fico_range_high+data$last_fico_range_low)/2
data$average_fico<-(data$fico_range_high+data$fico_range_low)/2
head(data$average_fico)
head(data$average_fico_last)

# Response Take "Charged off" and "Default" as default
table(data$loan_status)
data$default=ifelse(data$loan_status=="Current" 
                    | data$loan_status=="Fully Paid" 
                    | data$loan_status=="Issued" | 
                      data$loan_status=="In Grace Period",0,1)
table(data$default,data$loan_status)
table(data$default)
length(data$default)


#------------------------------------- Process Text data #----------------------------------------
install.packages("tm")
library(tm)
head(data[,c("title","emp_title","desc")])

# Process Loan title
corpus <-Corpus(VectorSource(data$title))
corpus <- tm_map(corpus, tolower) # lower case
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus<- tm_map(corpus, PlainTextDocument)
dtm_title<-DocumentTermMatrix(corpus,control = list(stemming = stemDocument))
dtm_title<-removeSparseTerms(dtm_title, 0.99)
dtm_title<-as.data.frame(as.matrix(dtm_title))
rownames(dtm_title) <- NULL
names(dtm_title)<-paste(names(dtm_title),sep=" ","title")
names(dtm_title)

## Loanee desc NLP

corpus <-Corpus(VectorSource(data$desc))
corpus <- tm_map(corpus, tolower) # lower case
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english')) # from library
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus<- tm_map(corpus, PlainTextDocument)
dtm_desc<-DocumentTermMatrix(corpus,control = list(stemming = stemDocument))
dtm_desc<-removeSparseTerms(dtm_desc, 0.99)
dtm_desc<-as.data.frame(as.matrix(dtm_desc))
rownames(dtm_desc) <- NULL
names(dtm_desc)<-paste(names(dtm_desc),sep=" ","desc")
names(dtm_desc)

#-----------------------------------------------------------------------------------------------
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

tmp1 = apply(data[getCharColumns(data)],2,function(x){length(unique(x))})
tmp1 = tmp1[tmp1==1]
str(data[getCharColumns(data)])# No charactor features left

str(data[getFactorColumns(data)]) # there is 6 factor predictors

dim(data[getNumericColumns(data)]) # 50 numeric predictors
tmp2 = apply(data[getNumericColumns(data)],2,function(x){(sd(x))})
tmp2 = tmp2[tmp2<0.1]
names(tmp2) 
# Home_ownership_dummy_OtherNone and home_ownership_dummy_intercept have standard variance less than 0.1,
#remove these predictors as they have little change


discard_column = c(names(tmp1),names(tmp2))
discard_column


#Discard columnss that have zero variance
dim(data)
test2 =data[,!(names(data) %in% discard_column )]
dim(test2)
data = test2
str(data)


#Drop features that have used to geneate new predictors
drop_notuse = c("fico_range_high","fico_range_low","last_fico_range_high","last_fico_range_low","issue_d","earlist_cr_line","last_pymnt_d",
                "next_pymnt_d","last_credit_pull_d","loan_status","home_ownership")
# Avearge fico and average fico last will makes the design matrix singular
# As response-default is transformed from loan_status, thus I have to drop loan status
test3 = data[,!(names(data) %in% drop_notuse)]
dim(test3)
str(test3)
data = test3
str(data)


# data_factor is the dataframe only include factor data
data_factor = data[getFactorColumns(data)]
str(data_factor)
dim(data_factor)# 4 factor variables


data_num = data[getNumericColumns(data)] # 44 numeric variables left
str(data_num)
dim(data_num)

# Write cleaned numeric data into Loan_num.csv file
write.csv(data_num,
          file="D:\\001UNC Charlotte\\2017Fall\\DSBA6156\\Individual_Project\\Loan_num.csv",row.names = F)
# Write cleaned factor data into Loan_factor.csv file
write.csv(data_factor,
          file="D:\\001UNC Charlotte\\2017Fall\\DSBA6156\\Individual_Project\\Loan_factor.csv",row.names = F)
# Write cleaned all data into LendingClub_v0.csv fil
write.csv(data,
          file="D:\\001UNC Charlotte\\2017Fall\\DSBA6156\\Individual_Project\\LendingClub_v0.csv",row.names = F)

#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------










# Imputate missing values in the remaining columns
#missForest
install.packages("missForest")
library(missForest)


#impute missing values, using all parameters as default values
data_num_imp <- missForest(data_num_mis)

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
