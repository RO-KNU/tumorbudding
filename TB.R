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
library(survival)
library(psych)
library(pROC)
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
## 1st step of feature extraction, Logistic regression

logpvalue=as.data.frame(matrix(,nrow=0,ncol=2))
logpvalue
j=0
for (i in 1:2074){
  eval(parse(text=paste("out=glm(TB~",colnames(dat_train)[i],",family=binomial,data=dat_train)")))
  if(coef(summary(out))[2,'Pr(>|z|)']<0.05){
    j=j+1
    logpvalue[j,1]=coef(summary(out))[2,'Pr(>|z|)']
    logpvalue[j,2]=colnames(dat_train)[i]  
  }
}
train_fs_lr<-dat_train[,logpvalue[,2]]

## 2nd step of feature selection: LASSO regularization

x.train1<-as.matrix(train_fs_lr[idx,])
y.train1<-as.matrix(TB$TB[idx])

cv.lasso<-cv.glmnet(x.train1,y.train1,alpha=1, nfolds=3,family='binomial',type.measure='auc')
lasso.coef=predict(cv.lasso,type='coefficients',s=cv.lasso$lambda.min)
train_lasso<-names(train_fs_lr)[lasso.coef@i]

dat_lasso<-fs.train[,as.matrix(fs_lr.train.lasso)] 

dat_lasso$TB<-TB$TB

fs_train<-dat_lasso[idx,]
fs_test<-dat_lasso[-idx,]

## Modeling
### Logistic regression
logi_model<-glm(TB~. , data=fs_train,family='binomial')
pred.test<-predict(logi_model,newdata=fs_test[c(1:(ncol(fs_test)-1))],type="response")
confusionMatrix(as.factor(fs_test$TB),as.factor(pred.test))

### Support vector machine
svm_tune_grid <- expand.grid(C=c(0.001,0.01,1,10,100), sigma=c(0.1,0.5,1,2,3,4,5))
ctl <- trainControl(method = 'repeatedcv', number = 3, repeats = 3, allowParallel=T, savePredictions='final')
fit.svm <- train(as.factor(TB)~., data=x.train, method='svmRadial', trControl=ctl,tuneGrid=svm_tune_grid) 
svm.model<-ksvm(TB~.,data=fs_train,kernel="rbfdot",C=3)
pred.test<-predict(svm.model,newdata=fs_test,type='response')
test.svm.roc=roc(fs_test$TB,as.numeric(pred.test))
confusionMatrix(as.factor(fs_test$TB),as.factor(pred.test))

### Random forest
rf_tune_grid <- expand.grid(mtry=1:6)
ctl <- trainControl(method = 'repeatedcv', number = 3, repeats = 3, allowParallel=T, savePredictions='final')
fit.rf <- train(as.factor(TB)~., data=x.train, method='rf', trControl=ctl,tuneGrid=rf_tune_grid) 
rf.model<-randomForest(TB~.,ntree=400,mtry=5,data=fs_train)
train.rf.roc=roc(fs_train$TB,pred.train)
pred.test<-predict(rf.model,newdata=fs_test,type='response')
caret::confusionMatrix(as.factor(fs_test$TB),as.factor(pred.test))

### 5-4 Neural Network
nn_tune_grid <- expand.grid(size = seq(from = 1, to = 10, by = 1),decay = c(0.5,0.1,0.01,0.001))
ctl <- trainControl(method = 'repeatedcv', number = 3, repeats = 3, allowParallel=T, savePredictions='final')
fit.nn <- train(as.factor(TB)~., data=x.train, method='nnet', trControl=ctl,tuneGrid=nn_tune_grid) 
nn.model<-neuralnet(TB~.,data=fs_train,hidden=2,algorithm='rprop+')
pred.test<-predict(nn.model,newdata=fs_test,type='response')
test.nn.roc=roc(fs_test$TB,as.numeric(pred.test))
confusionMatrix(as.factor(fs_test$TB),as.factor(pred.test))
