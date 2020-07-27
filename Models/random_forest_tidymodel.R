library(tidyverse)
library(tidymodels)
library(ggplot2)
air <-read_csv("data.csv")


# Split data
set.seed(1234)
trees_split <- initial_split(air, strata = Temperature)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)


# Data Pre-processing
tree_rec <- recipe(PM25 ~ ., data = trees_train)
tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)
juiced


# random forest
 tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune(),
) %>% 
  set_mode("regression") %>% 
  set_engine("ranger")

 
 
# workflow
 tune_wf <- workflow() %>% 
   add_recipe(tree_rec) %>% 
   add_model(tune_spec)

 
# train hyperparameter
 set.seed(1234)
trees_fold<-  vfold_cv(trees_train)
doParallel::registerDoParallel() 

set.seed(1234)
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_fold,
  grid = 20
)


#Visualization
tune_res %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse" ) %>% 
  select(mean, min_n, mtry) %>% 
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>% 
  ggplot(aes(value, mean, color = parameter)) +
    geom_point(show.legend = F) +
    facet_wrap(~ parameter, scales = "free_x")


# Tune again

rf_grid <- grid_regular(
  mtry(range = c(2,7)),
  min_n(range = c(2,10)),
  levels = 5
)
set.seed(134)
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_fold,
  grid = rf_grid
)



tune_res %>% 
  collect_metrics() %>% 
  filter(.metric == "rsq") %>% 
  mutate(min_n =factor(min_n)) %>% 
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point()


best_rsq <- select_best(tune_res, "rsq")


final_rf <- finalize_model(tune_spec, best_rsq )


#feature Importence

final_rf %>% 
  set_engine("ranger", importance = "permutation") %>% 
  fit(PM25 ~ .,
      data= juice(tree_prep)) %>%
  vip(geom = "col")
    


final_wf <- workflow() %>% 
  add_recipe(tree_rec) %>% 
  add_model(final_rf)

final_res <- final_wf %>% 
  last_fit(trees_split)


final_res %>% 
  collect_metrics()



