library(MASS) 
library(Metrics)
library(corrplot)
library(randomForest)
library(lars)
library(ggplot2)
library(xgboost)
library(Matrix)
library(methods)
library(caret)
library(tidyverse)
library(mlr)
library(data.table)
library(caret)
#install.packages("mlr")

air <- read.csv(file = "orginal.csv")
air <- air[,2:11]
str(air)
summary(air)
# train <- read.csv(file = "train.csv")
# train <- train[,2:11]
# test <- read.csv(file = "test.csv")
# test <- test[,2:11]

set.seed(1234)
ind <- sample(2, nrow(air),replace = TRUE, prob = c(0.7, 0.3))
train <- air[ind==1,]
test <- air[ind==2,]

set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
train <- air[ind==1,1:10]
test <- air[ind==2, 1:10]
t_train <- setDT(train) 
t_test <- setDT(test)
labels <- air[ind==1, 11]
ts_labels  <- air[ind==2, 11]
# labels <- train$PM25
# ts_labels <- test$PM25
# new_tr <- model.matrix(~.+0,data = train[,-c("PM25"),with=F]) 
# new_ts <- model.matrix(~.+0,data = test[,-c("PM25"),with=F])
# labels <-as.numeric(labels)-1
# ts_labels <- as.numeric(ts_labels)-1
dtrain <- xgb.DMatrix(label = labels, data = as.matrix(train))
dtest <- xgb.DMatrix(label =ts_labels, data = as.matrix(test))
set.seed(123)
xgbFit=xgboost(data= dtrain,
               nfold=5,label=labels,
               nrounds=2200,verbose=FALSE,objective='reg:linear',
               eval_metric='mae',nthread=8,eta=0.01,gamma=0.0468,
               max_depth=6,min_child_weight=1.7817,
               subsample=0.5213,
               colsample_bytree=0.4603)
print(xgbFit)


preds2 <- exp(predict(xgbFit,newdata=dtest)) - 1


mat <- xgb.importance (feature_names = colnames(dtrain),model = xgbFit)
xgb.plot.importance (importance_matrix = mat[1:10]) 

