library(tidyverse)
library(tidymodels)

model_data <- read_csv("model_data.csv")



# Create Model --------------------------------------------------------------------------------

#split
set.seed(1234)
splitz<- model_data %>% 
  initial_split()

train <- training(splitz)
test <- testing(splitz)

#Linear Model
lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")

#fit
lmfit<- lm_spec %>% 
  fit(pm25~., data=train)


# randomForest model
rf_spec <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

#fit
rf_fit <- rf_spec %>% 
  fit(pm25 ~ ., data=train)

# Result --------------------------------------------------------------------------------------

# result of model in trainingset
result_train<- lm_fit %>% 
  predict(new_data=train) %>% 
  mutate(truth=train$pm25,
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data=train) %>% 
              mutate(truth=train$pm25,
                     model = "rf"))

# result of model in testset
result_test<- lm_fit %>% 
  predict(new_data=test) %>% 
  mutate(truth=test$pm25,
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data=test) %>% 
              mutate(truth=test$pm25,
                     model = "rf"))

# Find Error Train
result_train %>% 
  group_by(model) %>% 
  rmse(truth=truth, estimate= .pred)


# Find error test
result_test %>% 
  group_by(model) %>% 
  rmse(truth=truth, estimate= .pred)

# Visualize Prediction of 2 models
result_test %>% 
  mutate(train="testing") %>% 
  bind_rows(result_train %>% 
              mutate(train="training")) %>% 
  ggplot(aes(truth,.pred, color=model))+
  geom_abline(lty = 2, color="gray80",size=1.5)+
  geom_point(alpha=0.5)+
  facet_wrap(~train)


# Cross Validation ----------------------------------------------------------------------------

air_folds<- vfold_cv(train)

rf_res<- fit_resamples(
  pm25~.,
  rf_spec,
  air_folds,
  control = control_resamples(save_pred = T)
)

# Result
rf_res %>% 
  collect_metrics()
 
# Visualization
rf_res %>% 
  unnest(.predictions) %>% 
  ggplot(aes(pm25, .pred, color=id))+
  geom_abline(lty = 2, color="gray80",size=1.5)+
  geom_point(alpha=0.5)+facet_wrap(~id)
  