creditcard <- read.csv(file.choose()) # Choose the credit card Data set
summary(creditcard)
str(creditcard)
attach(creditcard)

# Using imputation to check any missing value
sum(is.na(creditcard)) # No missing value observed in any variable

creditcard <- creditcard[,-1] # Removing the first column which is is an Index
View(creditcard)

library(psych)
pairs.panels(creditcard,col="red")

# Splitting the data into Traing & Testing (70/30 ratio)

install.packages("caTools")
library(caTools)

split<-sample.split(creditcard$card,SplitRatio = 0.70)
split
table(split) # F=396  T=923  , just to verify the ratio
creditcard.train<-subset(creditcard,split==TRUE)
creditcard.test<-subset(creditcard,split==FALSE)

# Build Logistic Regression Model on Train Data
model_creditcard_train<-glm(card~.,data = creditcard.train,family = "binomial")
summary(model_creditcard_train)  # Null Dev=982.55 & Res Dev=10164.31, AIC=10188
# Rule : Null Deviance should always be grater than Residual deviance
# As above model is not satisfying the above rule

# Build "Model_1" by removing "reports" variable
model_1_creditcard_train<-glm(card~.-reports ,data=creditcard.train,family = "binomial")
summary(model_1_creditcard_train) # Null Dev=982.55 & Res Dev=92.08, AIC=114.09
# But only 1 variable seems significant ; rest all 9 variables insignificant.

# Build "Model_2" by removing "age" variable
model_2_creditcard_train<-glm(card~.,data=creditcard.train[,-c(3)],family = "binomial")
summary(model_2_creditcard_train) # Null Dev=982.55 & Res Dev=10020.14, AIC=10042
# Null Dev is not grater than Residual Dev, not meeting Rule.

# Build "Model_3" by removing "income" variable
model_3_creditcard_train<-glm(card~.,data=creditcard.train[,-c(4)],family = "binomial")
summary(model_3_creditcard_train) # Null Dev=982.55 & Res Dev=9803.87, AIC=9825.9
# Null Dev is not grater than Residual Dev, not meeting Rule.

# Build "Model_4" by removing "share" variable
model_4_creditcard_train<-glm(card~.-share,data=creditcard.train,family = "binomial")
summary(model_4_creditcard_train) # Null Dev=982.55 & Res Dev=75.09, AIC=97.09
# But only 3 variables seems significant ; rest all 7 variables insignificant.

# Build "Model_5" by removing "expenditure" variable
model_5_creditcard_train<-glm(card~.,data=creditcard.train[,-c(6)],family = "binomial")
summary(model_5_creditcard_train) # Null Dev=982.55 & Res Dev=95.55, AIC=117.55
# 5 variables seems significant ; rest all 5 variables insignificant.

# Build "Model_6" by removing "owner" variable
model_6_creditcard_train<-glm(card~.,data=creditcard.train[,-c(7)],family = "binomial")
summary(model_6_creditcard_train) # Null Dev=982.55 & Res Dev=11173.53, AIC=11196
# Null Dev is not grater than Residual Dev, not meeting Rule.

# Build "Model_7" by removing "selfemp" variable
model_7_creditcard_train<-glm(card~.,data=creditcard.train[,-c(8)],family = "binomial")
summary(model_7_creditcard_train) # Null Dev=982.55 & Res Dev=7641.25, AIC=7663.3
# Null Dev is not grater than Residual Dev, not meeting Rule.

# Build "Model_8" by removing "dependents" variable
model_8_creditcard_train<-glm(card~.,data=creditcard.train[,-c(9)],family = "binomial")
summary(model_8_creditcard_train) # Null Dev=982.55 & Res Dev=10668.92, AIC=10691
# Null Dev is not grater than Residual Dev, not meeting Rule.

# Build "Model_9" by removing "months" variable
model_9_creditcard_train<-glm(card~.,data=creditcard.train[,-c(10)],family = "binomial")
summary(model_9_creditcard_train) # Null Dev=982.55 & Res Dev=9155.09, AIC=9177
# Null Dev is not grater than Residual Dev, not meeting Rule.

# Build "Model_10" by removing "majorcards" variable
model_10_creditcard_train<-glm(card~.,data=creditcard.train[,-c(11)],family = "binomial")
summary(model_10_creditcard_train) # Null Dev=982.55 & Res Dev=11533, AIC=11556
# Null Dev is not grater than Residual Dev, not meeting Rule.

# Build "Model_11" by removing "active" variable
model_11_creditcard_train<-glm(card~.,data=creditcard.train[,-c(12)],family = "binomial")
summary(model_11_creditcard_train) # Null Dev=982.55 & Res Dev=9299, AIC=9321
# Null Dev is not grater than Residual Dev, not meeting Rule.

# Build "Model_12" by removing "reports,share,expenditure" variables
model_12_creditcard_train<-glm(card~.,data=creditcard.train[,-c(2,5,6)],family = "binomial")
summary(model_12_creditcard_train) # Null Dev=982.55 & Res Dev=930.81, AIC=948.81
# 6 variables significant , 2 variables not significant

# Build "Model_13" by removing "reports,expenditure" variables
model_13_creditcard_train<-glm(card~.,data=creditcard.train[,-c(2,6)],family = "binomial")
summary(model_13_creditcard_train) # Null Dev=982.55 & Res Dev=115.70, AIC=135.7
# 3 variables significant , 6 variables not significant

# Build "Model_14" by removing "share,expenditure" variables
model_14_creditcard_train<-glm(card~.,data=creditcard.train[,-c(5,6)],family = "binomial")
summary(model_14_creditcard_train) # Null Dev=982.55 & Res Dev=684.36, AIC=704.36
# 8 variables significant , 1 variables not significant

# Build "Model_15" by removing "expenditure,owner,selfemp" variables
model_15_creditcard_train<-glm(card~.,data=creditcard.train[,-c(6,7,8)],family = "binomial")
summary(model_15_creditcard_train) # Null Dev=982.55 & Res Dev=95.83, AIC=113.84
# 5 variables significant , 3 variables not significant

# Build "Model_16" by removing "expenditure,owner,selfemp" variables
model_16_creditcard_train<-glm(card~.,data=creditcard.train[,-c(6,9,10)],family = "binomial")
summary(model_16_creditcard_train) # Null Dev=982.55 & Res Dev=103.68 , AIC=121.68 
# 4 variables significant , 4 variables not significant

# Build "Model_17" by removing "expenditure,owner,selfemp" variables
model_17_creditcard_train<-glm(card~.,data=creditcard.train[,-c(6,11)],family = "binomial")
summary(model_17_creditcard_train) # Null Dev=982.55 & Res Dev=96.76 , AIC=116.77 
# 5 variables significant , 4 variables not significant

# Based on the summary of all above models, 
# below models selected for further analysis :
# model_4 , model_14 , model_15

#Prediction & Accuracy of Model_4 on Train Data
prob_train_4<-model_4_creditcard_train$fitted.values
confusion_train_4<-table(prob_train_4>0.5,creditcard.train$card) # threshold 0.5
confusion_train_4
Accuracy_train_4<-sum(diag(confusion_train_4))/sum(confusion_train_4)
Accuracy_train_4 # 0.9837

#Prediction & Accuracy of Model_14 on Train Data
prob_train_14<-model_14_creditcard_train$fitted.values
confusion_train_14<-table(prob_train_14>0.5,creditcard.train$card) # threshold 0.5
confusion_train_14
Accuracy_train_14<-sum(diag(confusion_train_14))/sum(confusion_train_14)
Accuracy_train_14 # 0.8537

#Prediction & Accuracy of Model_15 on Train Data
prob_train_15<-model_15_creditcard_train$fitted.values
confusion_train_15<-table(prob_train_15>0.5,creditcard.train$card) # threshold 0.5
confusion_train_15
Accuracy_train_15<-sum(diag(confusion_train_15))/sum(confusion_train_15)
Accuracy_train_15 # 0.9804

# Based on Accuracy ; Model_4 considered for further analysis

install.packages("ggplots")
library(ROCR)
rocrpred_train<-prediction(prob_train_4,creditcard.train$card)
rocrperf_train<-performance(rocrpred_train,'tpr','fpr')
str(rocrperf_train)

plot(rocrperf_train,colorize=T,text.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.1,by=0.1))

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
rocr_cutoff_train <- data.frame(cut_off = rocrperf_train@alpha.values[[1]],fpr=rocrperf_train@x.values,tpr=rocrperf_train@y.values)
colnames(rocr_cutoff_train) <- c("cut_off","FPR","TPR")
View(rocr_cutoff_train)
#Rounding of values to 2 decimals
rocr_cutoff_train<-round(rocr_cutoff_train,2)

install.packages("dplyr")
library(dplyr)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff_train <- arrange(rocr_cutoff_train,desc(TPR))
View(rocr_cutoff_train)

# Based on TPR/FPR table, cutoff at 0.38 decided
# Check Accuracy with 0.38 cutoff on Model_4
confusion_train_4new<-table(prob_train_4>0.38,creditcard.train$card) 
confusion_train_4new
Accuracy_train_4new<-sum(diag(confusion_train_4new))/sum(confusion_train_4new)
Accuracy_train_4new # 0.9869

install.packages("pROC")
library(pROC)
auc_train<-performance(rocrpred_train,measure = "auc")
auc_train<-auc_train@y.values[(1)]
auc_train # 0.9973 means outstanding
str(auc_train)

#Prediction & Accuracy of Model_4 on Test Data
prob_test_4<-predict(model_4_creditcard_train,newdata=creditcard.test)
confusion_test<-table(prob_test_4>0.38,creditcard.test$card) 
confusion_test
Accuracy_test<-sum(diag(confusion_test))/sum(confusion_test)
Accuracy_test #0.9823

############ END ##################################333333 