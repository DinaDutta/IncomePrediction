#Set Working Directory
setwd("~/DataAnalytics/IncomePrediction")
#Load datasets
train<-read.csv("train_gbW7HTd.csv")
test<-read.csv("test_2AFBew7.csv")
#Observe data in train set
str(train)
#Divide train set into two categories - Categorical and Continuous
train_cont<-subset(train,select=c(ID,Age,Hours.Per.Week))
train_cat<-subset(train,select=-c(ID,Age,Hours.Per.Week))
#Lets look at the summary of the continuous variables
summary(train_cont)
#To observe detailed summary, we need to install pastecs package
install.packages("pastecs")
#Load the package
library("pastecs")
#get detailed summary after setting significant digits
options(scipen = 100)
options(digits = 2)
stat.desc(train_cont)
#Number of unique variables in Categorical variables
apply(train_cat,2,function(x){length(unique(x))})
#See count of each category
table(train_cat$Workclass)
table(train_cat$Education)
table(train_cat$Marital.Status)
table(train_cat$Occupation)
table(train_cat$Relationship)
table(train_cat$Race)
table(train_cat$Sex)
table(train_cat$Native.Country)
#See %age of observation in each category
as.matrix(prop.table(table(train_cat$Race)))
as.matrix(prop.table(table(train_cat$Workclass)))
as.matrix(prop.table(table(train_cat$Education)))
as.matrix(prop.table(table(train_cat$Marital.Status)))
as.matrix(prop.table(table(train_cat$Occupation)))
as.matrix(prop.table(table(train_cat$Relationship)))
as.matrix(prop.table(table(train_cat$Sex)))
#Print top20 countries
head(sort(table(train_cat$Native.Country), decreasing = TRUE),20)
#%age of observations in top 20 countries
head(round(sort(prop.table(table(train_cat$Native.Country)),decreasing = TRUE),6),20)
#Looking at dependency, Sex and Income Group - both categorical
#installing and loading package gmodels and then use CrossTable function
install.packages("gmodels")
library("gmodels")
CrossTable(train$Sex,train$Income.Group)
#Plotting the observations using ggplot2
library("ggplot2")
ggplot(train,aes(Sex,fill=Income.Group))+geom_bar()+labs(title="Stacked chart",x="Sex",y="Count")+theme_bw()
#Looking at dependency , Sex and Hours.Per.Week - Categorical and Continuous, plotting bar chart
ggplot(train,aes(Sex,Hours.Per.Week))+geom_boxplot()+labs(title="Box plot")
#Missing value treatment
#Check missing values in train set
table(is.na(train))
table(is.na(test))
#Outlier treatment for continuous variables
ggplot(train,aes(ID,Age))+geom_jitter()
ggplot(train,aes(ID,Hours.Per.Week))+geom_jitter()
#Variable Transformation
#Combining categories with less than 5% 
library("car")
#Using recode function from car package
train$Workclass<-recode(train$Workclass,"c('Federal-gov','Never-worked','Self-emp-inc','State-gov','Without-pay')='Others'")
as.matrix(prop.table(table(train$Workclass)))
test$Workclass<-recode(test$Workclass,"c('Federal-gov','Never-worked','Self-emp-inc','State-gov','Without-pay')='Others'")
train$Education<-recode(train$Education,"c('10th','11th','12th','1st-4th','5th-6th','7th-8th','9th','Assoc-acdm','Assoc-voc','Preschool','Prof-school')='Others'")
test$Education<-recode(test$Education,"c('10th','11th','12th','1st-4th','5th-6th','7th-8th','9th','Assoc-acdm','Assoc-voc','Preschool','Prof-school')='Others'")
as.matrix(prop.table(table(train$Education)))
as.matrix(prop.table(table(test$Education)))
#Predictive Modelling
#Data preprocessing
#Categorise dependent variable as 1 or 0
train$Income.Group<-ifelse(train$Income.Group=='<=50K',0,1)
#Removing identifier variable from our dataset
train<-subset(train,select=-c(ID))
#Building decision tree model
library("rpart")
set.seed(333)
tree<-rpart(Income.Group~.,data=train,method="class",control = rpart.control(minsplit = 20,minbucket = 100,maxdepth = 10), xval = 5)
summary(tree)
#Plot the model
library("rpart.plot")
rpart.plot(tree)
#Prediction for train set
pred_train<-predict(tree,newdata = train,type="class")
#Prediction for test set
pred_test<-predict(tree,newdata = test,type="class")
#Lets use Confusion matrix to evaluate our model since it is a classification problem
#To do so lets load caret and lattice(the former requires later) package
library("lattice")
library("caret")
confusionMatrix(pred_train,train$Income.Group)
#Create Solution file
solution<-data.frame(ID=test$ID,Income.Group=pred_test)
#Write it in a csv 
write.csv(solution,file="final_solution.csv")

















