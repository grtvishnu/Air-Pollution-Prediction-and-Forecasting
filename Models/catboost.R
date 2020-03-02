 # install.packages('devtools')
 # library(devtools)
 # devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
 # install.packages("catboost")
library(catboost)
library(caret)
library(dplyr)
library(plotly)
air <- read.csv(file = "orginal_1.csv")
# air <- air[,2:11]
str(air)
# air$PM25 <- as.numeric(air$PM25)
# labels <- train$PM25

set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(0.8, 0.2))
train <- air[ind==1,]
test <- air[ind==2,]

y_train <- unlist(train[c('PM25')])
X_train <- train %>% select(-PM25)
y_valid <- unlist(test[c('PM25')])
X_valid <- test %>% select(-PM25)


train_pool <- catboost.load_pool(data = X_train, label = y_train)
test_pool <- catboost.load_pool(data = X_valid, label = y_valid)


params <- list(iterations=1500,
               learning_rate=0.01,
               depth=10,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 1,
               od_wait=20,
               use_best_model=TRUE)




model <- catboost.train(learn_pool = train_pool,params = params)

catboost.get_model_params(model)

#predict
y_pred=catboost.predict(model,test_pool)
postResample(y_pred,test$PM25)

rmse(log(test$PM25),log(y_pred))
RMSE(y_pred, test$PM25, na.rm = T)
#saveRDS(model, "catboost_model_chennai.RDS")



#feature importence

importance <- catboost.get_feature_importance(model, 
                                pool = NULL, 
                                type = 'FeatureImportance',
                                thread_count = 6)

importance <- varImp(model, scale = FALSE)
print(importance)

