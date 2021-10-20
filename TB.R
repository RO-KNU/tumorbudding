# Description
#  This scriptes about one of radiomics research project in our D-lab. 
#  This code is dealing with several commonly used machine learning algorithms.
#  The purpose of modeling is to predict tumor budding (TB) status preoperatively
#    using imaging biomarkers in cervical cancer patients.

# Author(s):
#  Shin-hyung Park, M.D. <shinhyungpark@knu.ac.kr>

# History:
#   First upload on public repository:Sep/25/2021

# Disclaimer:
#   "I am a medical doctor (more specifically, a radiation oncologist) who has much interest on data science."

# Statement:
#   This file is part of shpark's D-lab dedicated to serve our patients better treatment.
#   --> Copyright (C) 2021 Shin-Hyung Park
#       All rights reserved. 

  
library(caret)
library(glmnet)
library(kernlab) 
library(neuralnet)
library(randomForest) 


idx<-sample(1:74,74*.66)
T1 # dataframe of features from T1WI
T2 # dataframe of features from T2WI
TB # tumor budding status
imsi1<-T1[38:1074]
imsi2<-T2[38:1074]
names(imsi1)<-paste0(names(imsi1),".T1")
names(imsi2)<-paste0(names(imsi2),".T2")
dat<-cbind(imsi1,imsi2)
dat_train<-dat[idx,]

## Modeling
### Logistic regression
logi_model<-glm(TB~. , data=fs_train,family='binomial')
pred.test<-predict(logi_model,newdata=fs_test[c(1:(ncol(fs_test)-1))],type="response")
confusionMatrix(as.factor(fs_test$TB),as.factor(pred.test))

### Support vector machine
svm.model<-ksvm(ITB.g~.,data=x.train,kernel="rbfdot",C=3)
pred.test<-predict(svm.model,newdata=fs_test,type='response')
test.svm.roc=roc(fs_test$TB,as.numeric(pred.test))
confusionMatrix(as.factor(fs_test$TB),as.factor(pred.test))

### Random forest
rf.model<-randomForest(TB~.,ntree=400,mtry=5,data=fs_train)
train.rf.roc=roc(fs_train$TB,pred.train)
pred.test<-predict(rf.model,newdata=fs_test,type='response')
confusionMatrix(as.factor(fs_test$TB),as.factor(pred.test))

### 5-4 Neural Network
nn.model<-neuralnet(ITB.g~.,data=x.train,hidden=2,algorithm='rprop+')
pred.test<-predict(nn.model,newdata=fs_test,type='response')
test.nn.roc=roc(fs_test$TB,as.numeric(pred.test))
confusionMatrix(as.factor(fs_test$TB),as.factor(pred.test))
