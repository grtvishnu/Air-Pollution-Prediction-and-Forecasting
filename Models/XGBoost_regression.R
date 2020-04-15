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
train <- air[ind == 1, 1:8]
test <- air[ind == 2, 1:8]

t_train <- setDT(train)
t_test <- setDT(test)
labels <- air[ind == 1, 8]
ts_labels <- air[ind == 2, 8]

dtrain <- xgb.DMatrix(label = labels, data = as.matrix(train))
dtest <- xgb.DMatrix(label = ts_labels, data = as.matrix(test))

# Model
set.seed(123)
xgbFit <- xgboost(
  data = dtrain,
  nfold = 5, label = labels,
  nrounds = 2200, verbose = T, objective = "reg:linear",
  eval_metric = "rmse", nthread = 8, eta = 0.01, gamma = 0.0468,
  max_depth = 6, min_child_weight = 1.7817,
  subsample = 0.5213,
  colsample_bytree = 0.4603
)

# predict
print(xgbFit)
pred <- predict(xgbFit, dtest)
print(length(pred))
print(head(pred))
RMSE(pred, ts_labels)

# Feature Importance
importance_matrix <- xgb.importance(model = xgbFit)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
preds2 <- exp(predict(xgbFit, newdata = dtest)) - 1

mat <- xgb.importance(feature_names = colnames(dtrain), model = xgbFit)
xgb.plot.importance(importance_matrix = mat[1:10])

#Xgb tree
xgb.plot.tree(
  feature_names = NULL, model = xgbFit, trees = 10,
  plot_width = 1920, plot_height = 1080, render = TRUE,
  show_node_id = FALSE
)