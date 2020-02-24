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
# 
# set.seed(1234)
# ind <- sample(2, nrow(air),replace = TRUE, prob = c(0.7, 0.3))
# train <- air[ind==1,]
# test <- air[ind==2,]

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
               nrounds=2200,verbose=T,objective='reg:linear',
               eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,
               max_depth=6,min_child_weight=1.7817,
               subsample=0.5213,
               colsample_bytree=0.4603)
print(xgbFit)

pred <- predict(xgbFit, dtest)
print(length(pred))
print(head(pred))

importance_matrix <- xgb.importance(model = xgbFit)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
preds2 <- exp(predict(xgbFit,newdata=dtest)) - 1


mat <- xgb.importance (feature_names = colnames(dtrain),model = xgbFit)
xgb.plot.importance (importance_matrix = mat[1:10]) 

xgb.dump(xgbFit, with_stats = T)



xgb.plot.tree(feature_names = NULL, model = xgbFit, trees = 10,
              plot_width = 1920, plot_height = 1080, render = TRUE,
              show_node_id = FALSE)
