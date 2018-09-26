#import file
raw_data <- read.csv("F:/imarticus projects/dataset/data/datasets/CreditRisk.csv")
summary(raw_data)
#remove na
raw_data$Dependents[is.na(raw_data$Dependents)]<-mean(raw_data$Dependents,na.rm=TRUE)
raw_data$LoanAmount [is.na(raw_data$LoanAmount)]<-mean(raw_data$LoanAmount,na.rm=TRUE)
raw_data$Loan_Amount_Term[is.na(raw_data$Loan_Amount_Term)]<-mean(raw_data$Loan_Amount_Term,na.rm=TRUE)
raw_data$Credit_History[is.na(raw_data$Credit_History)]<-mean(raw_data$Credit_History,na.rm=TRUE)
dataset=raw_data
#install.packages("dplyr")
library(dplyr)
dataset<-mutate(dataset,Gender1=ifelse(Gender=="Male",0,1))
dataset<-mutate(dataset,Married1=ifelse(Married=="No",0,1))
dataset<-mutate(dataset,Education1=ifelse(Education=="Graduate",0,1))
dataset<-mutate(dataset,Self_Employed1=ifelse(Self_Employed=="No",0,1))
dataset<-mutate(dataset,Property_Area1=ifelse(Property_Area=="Urban",0,ifelse(Property_Area=="Rural",1,2)))
dataset<-mutate(dataset,Loan_Status1=ifelse(Loan_Status=="Y",0,1))
dataset<-dataset[ ,c(4,7:11,14:19)]
View(dataset)

#sampling

CRS<-sample(2,nrow(dataset),replace=TRUE,prob=c(.9,.1))
train_CRS<-dataset[CRS==1,]
test_CRS<-dataset[CRS==2,]


#logistic regression
model_glm<-glm(Loan_Status1~.,family = binomial,data=train_CRS)
pred_value<-predict(model_glm,test_CRS,type = "response")
pred_value
logistic<-ifelse(pred_value>.5,1,0)

#random forest
#install.packages("randomForest")
library(randomForest)
model_rf<- randomForest(formula=Loan_Status1~., data = train_CRS)
pred_value1<- predict(model_rf, newdata = test_CRS)
randomforest<-ifelse(pred_value1>.5,1,0)

#  SVM 
#install.packages("e1071")
library(e1071)
model_svm<- svm(formula =Loan_Status1~., data = train_CRS)
pred_value2<-predict(model_svm,test_CRS,type = "response")
svm<-ifelse(pred_value2>.5,1,0)


# Decision Tree          

#install.packages("party")
library(party)
#install.packages("rpart")
library(rpart)
model_dt<- ctree(formula=Loan_Status1~., data =train_CRS,)
pred_value3<-predict(model_dt,test_CRS)
pred_value3
dicision<-ifelse(pred_value3>.5,1,0)
dicision

# Create a function to calculate and return the mode of values
getMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode<-apply(result,1,getMode)
result=data.frame(logistic,randomforest,svm,dicision,mode)
result

colnames(result) <- c("Logistic", "Random Forest", "SVM","Ctree","mode")
result



