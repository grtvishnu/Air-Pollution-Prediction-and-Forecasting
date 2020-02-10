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

air <- read.csv(file = "orginal_7.csv")
air <- air[,2:11]
str(air)
# train <- read.csv(file = "train.csv")
# train <- train[,2:11]
# test <- read.csv(file = "test.csv")
# test <- test[,2:11]

set.seed(1234)
ind <- sample(2, nrow(air),replace = TRUE, prob = c(0.7, 0.3))
train <- air[ind==1,]
test <- air[ind==2,]


t_train <- setDT(train) 
t_test <- setDT(test)
labels <- train$PM25
ts_labels <- test$PM25
# new_tr <- model.matrix(~.+0,data = train[,-c("PM25"),with=F]) 
# new_ts <- model.matrix(~.+0,data = test[,-c("PM25"),with=F])
# labels <-as.numeric(labels)-1
# ts_labels <- as.numeric(ts_labels)-1
dtrain <- xgb.DMatrix(label = labels, data = as.matrix(train))
dtest <- xgb.DMatrix(label =ts_labels, data = as.matrix(test))
params <- list(booster = "gbtree", objective = "reg:linear",
               eta=0.3,
               gamma=0,
               max_depth=6,
               min_child_weight=1,
               subsample=1,
               colsample_bytree=1)
xgbcv <- xgb.cv(params = params, data = dtrain,
                nrounds = 1000, nfold = 5,
                showsd = T,
                stratified = T,
                maximize = F)

min(xgbcv$evaluation_log)


xgb1 <- xgb.train (params = params, data = dtrain,
                   nrounds = 425,
                   watchlist = list(val=dtest,train=dtrain),
                   maximize = F ,
                   eval_metric = "error")

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
# confusionMatrix (xgbpred, ts_labels)

mat <- xgb.importance (feature_names = colnames(train),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:10]) 

