bank <- read.csv(file.choose()) # Choose the bank Data set
summary(bank)
str(bank)
colnames(bank)[17]<-"remark"  # change the column name for meaning of Y
attach(bank)

# Using imputation to check any missing value
sum(is.na(bank)) # No missing value observed in any variable

library(psych)
pairs.panels(bank,col="red")

# Splitting the data into Traing & Testing (70/30 ratio)

install.packages("caTools")
library(caTools)

split<-sample.split(bank$remark,SplitRatio = 0.70)
split
table(split) # F=13564  T=31647  , just to verify the ratio
bank_train<-subset(bank,split==TRUE)
bank_test<-subset(bank,split==FALSE)

# Build Logistic Regression Model on Train Data
model_bank<-glm(remark~.,data = bank_train,family = "binomial")
summary(model_bank)  # Null Dev=22840 & Res Dev=15116, AIC=15202

# Build "Model_1" by removing few insignificant variables
model_1_bank<-glm(remark~.,data=bank_train[,-c(1,5,14,15)],family = "binomial")
summary(model_1_bank) # Null Dev=22840 & Res Dev=15118, AIC=15196

# Build "Model_2" by removing few insignificant variables
model_2_bank<-glm(remark~.,data=bank_train[,-c(1)],family = "binomial")
summary(model_2_bank) # Null Dev=22840 & Res Dev=15116, AIC=15200


# Based on the summary of all above models, 
# below model selected for further analysis :
# model_1

#Prediction & Accuracy of Model_1 on Train Data
prob_train<-model_1_bank$fitted.values
confusion_train<-table(prob_train>0.5,bank_train$remark) # threshold 0.5
confusion_train
Accuracy_train<-sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train # 0.9016

install.packages("ggplots")
library(ROCR)
rocrpred_train<-prediction(prob_train,bank_train$remark)
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

# Based on TPR/FPR table, cutoff at 0.40 decided
# Check Accuracy with 0.40 cutoff on Model_1
confusion_train_new<-table(prob_train>0.40,bank_train$remark) 
confusion_train_new
Accuracy_train_new<-sum(diag(confusion_train_new))/sum(confusion_train_new)
Accuracy_train_new # 0.9034

install.packages("pROC")
library(pROC)
auc_train<-performance(rocrpred_train,measure = "auc")
auc_train<-auc_train@y.values[(1)]
auc_train # 0.9080 means outstanding
str(auc_train)

#Prediction & Accuracy of Model_1 on Test Data
prob_test<-predict(model_1_bank,newdata=bank_test)
confusion_test<-table(prob_test>0.40,bank_test$remark) 
confusion_test
Accuracy_test<-sum(diag(confusion_test))/sum(confusion_test)
Accuracy_test #0.9011

############ END ##################################333333 