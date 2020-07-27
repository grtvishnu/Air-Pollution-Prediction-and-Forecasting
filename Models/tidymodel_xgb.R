library(tidymodels)
library(tidyverse)
library(xgboost)
air <- read.csv("data.csv")
set.seed(1234)

splitz<- initial_split(air)
train <- training(splitz)
test <- testing(splitz)

xgb_spec<- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(), learn_rate = tune()
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")


xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size=sample_prop(),
  finalize(mtry(),train),
  learn_rate(),
  size = 20 # u can increase this for more grid
  )


xgb_wf <- workflow() %>% 
  add_formula(PM25 ~ .) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(1234)

air_fold <- vfold_cv(train)

air_fold

doParallel::registerDoParallel()

set.seed(1234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = air_fold,
  grid = xgb_grid,
  control = control_grid(save_pred = T)
)


# explore results

xgb_res %>% 
  collect_metrics() %>%
  filter(.metric=="rmse") %>% #rmse for regression "roc_auc" for classification 
  select(mean,mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               names_to = "parameter",
               values_to = "value") %>% 
  ggplot(aes(value,mean, color=parameter))+
  geom_point(show.legend = F)+
  facet_wrap(~parameter, scales = "free_x")


show_best(xgb_res, "rmse")

best_rmse<- select_best(xgb_res, "rmse")

final_xgb<- finalize_workflow(xgb_wf,best_rmse)

final_xgb


library(vip)

final_xgb %>% 
  fit(data =train) %>% 
  pull_workflow_fit() %>% 
  vip(geom ="point")


final_res<- last_fit(final_xgb, splitz)

final_res %>% 
  collect_metrics

final_res %>% 
  collect_predictions() %>% 
  rmse(truth=pm25, estimate= .pred)
