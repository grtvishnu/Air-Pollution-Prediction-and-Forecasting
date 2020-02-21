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
library(lubridate)
library(scales)
#install.packages("mlr")

air <- read.csv(file = "orginal.csv")
#air <- air[,2:11]
str(air)
summary(air)
# train <- read.csv(file = "train.csv")
# train <- train[,2:11]
# test <- read.csv(file = "test.csv")
# test <- test[,2:11]
# train$AQI <- log(train$AQI + 1)
# qplot(AQI, data = train, bins = 50, main = "Normal distribution after log transformation")
# train  %>% 
# ggplot(aes(PM10)) + geom_histogram(aes(fill = 600) ,bins=40)  + xlim(NA,180) + #hide outlier at 260
#   ggtitle('Distribution of Response Variable Y')
# top_10 <- mean_difference %>% slice(1:10)  %>% select(Variable,difference)
# train_df[,names(train_df) %in% top_10$Variable]  %>% bind_cols(as.data.frame(train_df$y))  %>% 
#   rename(y=`train_df$y`)  %>%  gather(Variable, Value, -y)  %>% mutate_at(vars(Value),factor)  %>% 
#   ggplot(aes(Value, y)) + geom_boxplot(aes(fill=Value, group=Value)) + 
#   facet_wrap(~Variable, scales = "free", ncol = 5) + scale_y_continuous(labels = comma) + xlab("")

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

#GRID SEARCH
searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6), 
                                colsample_bytree = c(0.5, 0.6),
                                max_depth = c(3, 4),
                                min_child = seq(1), 
                                eta = c(0.1)
)

ntrees <- 300

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = ntrees, nfold = 5, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))}))


output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames
head(output)

# write.csv(output, "xgb_gridsearch.csv")



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

