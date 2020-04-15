#Libraries
library(catboost)
library(caret)
library(dplyr)
library(plotly)
library(tidyverse)
library(MLmetrics)
# Read data
air <- read_csv(file = "data.csv")
str(air)
#Split data
set.seed(1234)
ind <- sample(2, nrow(air), replace = T, prob = c(0.8, 0.2))
train <- air[ind == 1, ]
test <- air[ind == 2, ]
# Create Label
y_train <- unlist(train[c("PM25")])
X_train <- train %>% select(-PM25)
y_valid <- unlist(test[c("PM25")])
X_valid <- test %>% select(-PM25)

# create train pool and test pool
train_pool <- catboost.load_pool(data = X_train, label = y_train)
test_pool <- catboost.load_pool(data = X_valid, label = y_valid)

# model
params <- list(
  iterations = 1500,
  learning_rate = 0.01,
  depth = 10,
  loss_function = "RMSE",
  eval_metric = "RMSE",
  random_seed = 55,
  od_type = "Iter",
  metric_period = 1,
  od_wait = 20,
  use_best_model = TRUE
)

# compile
model <- catboost.train(learn_pool = train_pool, params = params)

#Summary
catboost.get_model_params(model)

# model prediction
y_pred <- catboost.predict(model, test_pool)
postResample(y_pred, test$PM25)

rmse(log(test$PM25), log(y_pred))
RMSE(y_pred, test$PM25)

# feature importence
importance <- catboost.get_feature_importance(model,
                                              pool = NULL,
                                              type = "FeatureImportance",
                                              thread_count = 6
)

print(importance)


# plot importance
atta <- c("Temperature", "Windspeed", "Pressure", "NO2", "Rainfall", "PM10", "AQI")
impo<-cbind(atta, importance)
impo <- as.tibble(impo)
impo$V2 <- as.numeric(impo$V2)
impo

impo <- impo %>% 
  arrange(desc(V2))
impo

g <- ggplot(impo, aes(x = reorder(atta, V2), y = V2))+
  geom_bar(stat="identity")+
  coord_flip()+
  xlab("Variables")+
  ylab("Importance")
  
ggplotly(g)
  

