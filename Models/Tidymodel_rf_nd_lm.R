library(tidyverse)
library(tidymodels)

model_data <- read_csv("model_data.csv")

model_data <- model_data %>% 
  select(co:pm10,so2,pm25)


table(is.na(model_data))

ggplot(model_data,aes(pm25))+
  geom_density()


set.seed(1234)
splitz<- model_data %>% 
  initial_split()

train <- training(splitz)
test <- testing(splitz)

lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")


lmfit<- lm_spec %>% 
  fit(pm25~., data=train)



rf_spec <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")


rf_fit <- rf_spec %>% 
  fit(pm25 ~ ., data=train)


result_train<- lm_fit %>% 
  predict(new_data=train) %>% 
  mutate(truth=train$pm25,
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data=train) %>% 
              mutate(truth=train$pm25,
                     model = "rf"))


result_test<- lm_fit %>% 
  predict(new_data=test) %>% 
  mutate(truth=test$pm25,
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data=test) %>% 
              mutate(truth=test$pm25,
                     model = "rf"))



result_train %>% 
  group_by(model) %>% 
  rmse(truth=truth, estimate= .pred)



result_test %>% 
  group_by(model) %>% 
  rmse(truth=truth, estimate= .pred)



result_test %>% 
  mutate(train="testing") %>% 
  bind_rows(result_train %>% 
              mutate(train="training")) %>% 
  ggplot(aes(truth,.pred, color=model))+
  geom_abline(lty = 2, color="gray80",size=1.5)+
  geom_point(alpha=0.5)+
  facet_wrap(~train)


air_folds<- vfold_cv(train)


rf_res<- fit_resamples(
  pm25~.,
  rf_spec,
  air_folds,
  control = control_resamples(save_pred = T)
)

rf_res %>% 
  collect_metrics()
 

rf_res %>% 
  unnest(.predictions) %>% 
  ggplot(aes(pm25, .pred, color=id))+
  geom_abline(lty = 2, color="gray80",size=1.5)+
  geom_point(alpha=0.5)+facet_wrap(~id)
  