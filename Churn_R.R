#Remove all objects stored
rm(list=ls())

train_data = read.csv("D:\\Kaggle\\Churn_Reduction\\Train_data.csv")
test_data = read.csv("D:\\Kaggle\\Churn_Reduction\\Test_data.csv")

#first 5 columns of train_data
head(train_data,5)

#Shape of train data
dim(train_data)

#shape of test data
dim(test_data)

#first five columns of test data
head(test_data)

library(ggplot2)
library(corrgram)
library(ggpubr)

#count plot of churn
ggplot(train_data, aes_string(x = train_data$Churn)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Churn") + ylab('Count') +ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

#structure of training data
str(train_data)

#structure of test data
str(test_data)

#variables in the dataset
colnames(train_data)

#EDA
#There are no missing values in both of training and test data

#selecting only numeric varaibles
numeric_index = sapply(train_data,is.numeric) 
num_data=train_data[,numeric_index]
colnames=colnames(num_data)

#Correlation Plot 
corrgram(num_data, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#selecting only categorical data
factor_index = sapply(train_data,is.factor)
factor_data = train_data[,factor_index]

#Chi-squared Test of Independence
for (i in 1:4)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])))
}

## Dimension Reduction
train_data = subset(train_data, select = -c(phone.number,state,total.day.minutes,
                    total.eve.minutes,total.night.minutes,total.intl.minutes) )

test_data = subset(test_data, select = -c(phone.number,state,total.day.minutes,
                    total.eve.minutes,total.night.minutes,total.intl.minutes) )

##replacing value of Churn
train_data$Churn= as.numeric(train_data$Churn)
test_data$Churn= as.numeric(test_data$Churn)

#Bivariate Analysis of data

ggplot(train_data, aes_string(x=train_data$Churn, y=train_data$number.vmail.messages))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('Churn')+ylab("no of vmail")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

ggplot(train_data, aes_string(x=train_data$Churn, y=train_data$total.day.calls))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('Churn')+ylab("total day calls")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

ggplot(train_data, aes_string(x=train_data$Churn, y=train_data$total.eve.calls))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('Churn')+ylab("total eve calls")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

ggplot(train_data, aes_string(x=train_data$Churn, y=train_data$total.night.calls))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('Churn')+ylab("total night calls")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

ggplot(train_data, aes_string(x=train_data$Churn, y=train_data$total.intl.calls))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('Churn')+ylab("total intl calls")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

ggplot(train_data, aes_string(x=train_data$Churn, y=train_data$total.day.charge))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('Churn')+ylab("total day charge")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

ggplot(train_data, aes_string(x=train_data$Churn, y=train_data$total.eve.charge))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('Churn')+ylab("total eve charge")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

ggplot(train_data, aes_string(x=train_data$Churn, y=train_data$total.night.charge))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('Churn')+ylab("total night charge")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

ggplot(train_data, aes_string(x=train_data$Churn, y=train_data$total.intl.charge))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('Churn')+ylab("total intl charge")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

#Univariate Analysis

unique(train_data$number.customer.service.calls)
ggplot(train_data, aes_string(x=train_data$number.customer.service.calls))+
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  ylab('count')+xlab("total no of customer service calls")+ggtitle("Churn Analysis") +  
  theme(text=element_text(size=15))

#Histogram 
ggplot(train_data, aes_string(x = train_data$account.length)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  theme_bw() + xlab("Account length") + ylab("Frequency")  +
  theme(text=element_text(size=20))

ggplot(train_data, aes_string(x = train_data$total.day.calls)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  theme_bw() + xlab("total day calls") + ylab("Frequency") +
  theme(text=element_text(size=20))

ggplot(train_data, aes_string(x = train_data$account.length)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  theme_bw() + xlab("Account length") + ylab("Frequency") + 
  theme(text=element_text(size=20))

#most of the data are normalized

#Data type conversion
train_data$international.plan= as.numeric(train_data$international.plan)
test_data$international.plan= as.numeric(test_data$international.plan)
train_data$voice.mail.plan= as.numeric(train_data$voice.mail.plan)
test_data$voice.mail.plan= as.numeric(test_data$voice.mail.plan)
train_data$Churn= as.factor(train_data$Churn)
test_data$Churn= as.factor(test_data$Churn)


library(dummies)
train_data$area.code= as.factor(train_data$area.code)
test_data$area.code= as.factor(test_data$area.code)
train_data = cbind(train_data, dummy(train_data$area.code))
test_data = cbind(test_data, dummy(test_data$area.code))
train_data = subset(train_data, select = -c(train_data510,area.code))
test_data = subset(test_data, select = -c(test_data510,area.code))
names(train_data)[16]="Area415"
names(test_data)[16]="Area415"
names(train_data)[15]="Area408"
names(test_data)[15]="Area408"



##SAMPLING
#because of target class imbalance we will randomly oversample the minority class

train =subset(train_data, Churn==2)
samp1= train[sample(nrow(train), 400, replace = F), ]
samp2= train[sample(nrow(train), 400, replace = F), ]

trainer=rbind(train_data,samp1,samp2)

#shuffle the training data
trainer=trainer[sample(nrow(trainer),replace = F),]
tester=test_data

#Modelling
library(randomForest)
classifer= randomForest(x=trainer[-14],y=trainer$Churn,ntree=400)

#Presdict test data using random forest model
ypred = predict(classifer, newdata=tester[-14])

##Evaluate the performance of classification model
cm = table(tester[,14],ypred)

#False Negative rate
FNR = FN/FN+TP 

#Accuracy = 96.04
#FNR = 0.25





