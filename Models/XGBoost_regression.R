library(xgboost)
library(Matrix)
library(tidyverse)
library(data.table)
library(scales)
library(MLmetrics)

air <- read.csv(file = "data.csv")
str(air)
summary(air)

set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
train <- air[ind == 1, 1:7]
test <- air[ind == 2, 1:7]

t_train <- setDT(train)
t_test <- setDT(test)
labels <- air[ind == 1, 8]
ts_labels <- air[ind == 2, 8]

dtrain <- xgb.DMatrix(label = labels, data = as.matrix(train))
dtest <- xgb.DMatrix(label = ts_labels, data = as.matrix(test))

# Model
# set.seed(123)
# xgbFit <- xgboost(
#   data = dtrain,
#   nfold = 5, label = labels,
#   nrounds = 2200, verbose = T, objective = "reg:linear",
#   eval_metric = "rmse", nthread = 8, eta = 0.01, gamma = 0.0468,
#   max_depth = 6, min_child_weight = 1.7817,
#   subsample = 0.5213,
#   colsample_bytree = 0.4603
# )

xgb_new <- xgboost(data = dtrain,
               label = labels,
               trees = 1000,
               mtry = 8,
               min_n = 10,
               tree_depth = 3,
               learn_rate= 	0.00644471,
               loss_reduction = 0.000002150994,
               sample_size = 0.4017052,
               eval_metric ="rmse",
               nrounds = 1000)

# predict
print(xgb_new)
pred <- predict(xgbFit, dtest)
print(length(pred))
print(head(pred))
RMSE(pred, ts_labels)
aa <- cbind(ts_labels,pred)
# Feature Importance
importance_matrix <- xgb.importance(model = xgbFit)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#Xgb tree
xgb.plot.tree(
  feature_names = NULL, model = xgbFit, trees = 10,
  plot_width = 800, plot_height = 600, render = TRUE,
  show_node_id = FALSE
)

#save model

saveRDS(xgbFit, file = "xgb.rds")
xgbFit<-readRDS("xgb.rds")
