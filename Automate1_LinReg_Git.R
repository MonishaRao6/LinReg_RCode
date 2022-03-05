# Steps in linear regression

#1) import data 

getwd()
setwd("D:/Assignments/Extra/Prac/R_1")
my_data= read.csv("customer_churn.csv",header=TRUE,sep=',',stringsAsFactors = FALSE)

library(dplyr)


#importing all functions

source("LinRegFunctions.r")


#2) Perform EDA (exploratory data analysis)
#check datatype, remove unnecessary col,find missing value 
#and outliers

my_data<- subset(my_data, select=-c(customerID))
#my_data<- subset(my_data, select=-c(SeniorCitizen))
y=3  #m+3sd
view_col_summary(my_data,y) 

#treat datatype,missing val, outliers depending on y and quantity
#if too many, drop the column/variable, if less, replace it
# 2.a) treat missing value

#my_data<-NA2mean_num_all_col(my_data)
my_data<-NA2median_num_all_col(my_data)
#my_data<-NA2mode_num_all_col(my_data)

# check again if miss val fixed

view_col_summary(my_data,y)

# 2.b) treat outliers

#my_data<-Outlier2mean_num_all_col(my_data)
my_data<-Outlier2median_num_all_col(my_data,y)
#my_data<-Outlier2mode_num_all_col(my_data)

# check again if outliers fixed

view_col_summary(my_data,y)

# 3) Dimensionality Reduction: sd check
#if sd<0.1, drop the col.

my_data<-SD_num_all_col(my_data)
view_col_summary(my_data,y)

# 4) Dimensionality Reduction: linearity check
#if variable transformability can't fix, drop the col

library(ggplot2)  
my_data_1 = subset(my_data,select=-c(TotalCharges))#remove y as we don't need that plot
k=my_data$TotalCharges
plot_all_col(my_data_1,my_data,k)
#treat non linear plots
#variable transformability :#df$colnew = log(col)/sqrt(col)/col*col
#example

#my_data$tenure_log = log(my_data$tenure)


#5) corr between y and x1...xn are checked. if <0.3, drop it
#cor check
n=select(my_data,TotalCharges)
cor_all_col(my_data,my_data_1,n)

#5) split train-test 
library(caTools)
nrow(my_data)
sample.split(my_data$MonthlyCharges,SplitRatio = 0.65)-> split_tag
subset(my_data, split_tag==T)->train
subset(my_data, split_tag==F)->test
str(test)
nrow(train)
nrow(test)
# basic model
#lm(MonthlyCharges~tenure,data=train)->mdl1
#lm(TotalCharges~.,data=train)->mdl1
#lm(MonthlyCharges~Churn+TotalCharges+PaymentMethod+Contract+OnlineBackup+tenure,data=train)->mdl1
lm(MonthlyCharges~.,data=train)->mdl1

mdl1$coefficients
predict(mdl1, newdata=test)->predicted_values
summary(mdl1)
cbind(Actual=test$MonthlyCharges,Predicted=predicted_values)->final_data
head(final_data)

class(final_data)
as.data.frame(final_data)->final_data
class(final_data)

#error actual-pred
final_data$Actual - final_data$Predicted ->error
head(error)
cbind(final_data,error)-> final_data
head(final_data)

#combine 2 dataframe-- final data still matrix
class(final_data)
as.data.frame(final_data)->final_data
class(final_data)
#rmse
sqrt(mean((final_data$error)^2))->rmse
rmse
#mape
mean(abs(final_data$error)/abs(final_data$Actual))->mape
mape
#Adj R2
summary(mdl1)


#Another model
lm(MonthlyCharges~InternetService,data=train)->mdl2
mdl2$coefficients
predict(mdl2, newdata=test)->predicted_values
summary(mdl2)
cbind(Actual2=test$MonthlyCharges,Predicted2=predicted_values)->final_data2
head(final_data2)
class(final_data2)
as.data.frame(final_data2)->final_data2
#error actual-pred
final_data2$Actual2 - final_data2$Predicted2 ->error2
head(error2)
cbind(final_data2,error2)-> final_data2
head(final_data2)

#combine 2 dataframe-- final data still matrix
class(final_data2)
as.data.frame(final_data2)->final_data2
class(final_data2)
#rmse
sqrt(mean((final_data2$error2)^2))->rmse2
rmse2
#mape
mean(abs(final_data2$error2)/abs(final_data2$Actual))->mape2
mape2
#adj r2
summary(mdl2)


#plots - assumptions

plot(final_data$error)
plot(final_data$Predicted,final_data$error)
ggplot(final_data, aes(x=error)) + geom_histogram()
ggplot(final_data, aes(x=Predicted, y=error)) + geom_point()

plot(final_data2$error2)
plot(final_data2$Predicted2,final_data2$error2)
ggplot(final_data2, aes(x=error2)) + geom_histogram()
ggplot(final_data2, aes(x=Predicted2, y=error2)) + geom_point()
