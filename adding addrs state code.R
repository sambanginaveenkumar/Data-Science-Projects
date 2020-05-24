
post_imputation_data<-read.csv("Post Imputation.csv",header=TRUE)
Loan1<-read.csv("loan.csv",header=TRUE)

View(Loan1)
View(post_imputation_data)
names(Loan1)
names(post_imputation_data)

loan2<-Loan1[,c(1,9,24,26)]
post_imputation_data2<-post_imputation_data[,-c(1,8,16)]
final_data_set<-merge(post_imputation_data2,loan2,by="id")

which( colnames(final_data_set)=="total_pymnt_inv" )
which( colnames(final_data_set)=="member_id" )
which( colnames(final_data_set)=="id")

final_loan_data<-final_data_set[, -c(1,2,24)]
View(final_loan_data)
write.csv(final_loan_data, file = "Post Imputation final.csv")
