
# Loading Packages --------------------------------------------------------
library(tidyverse)
library(broom)
library(parsnip)
library(yardstick)
library(tidymodels)
library(janitor)
library(skimr)
library(lubridate)
library(xgboost)
library(kknn)
library(ranger)





# Boosted Trees Model- 1st Attempt ----------------------------------------
# Reading in Data
pike_volume_processed_data <- read_rds("pike_volume_processed_data.rds")



# Filtering out Data
pike_volume_data <- pike_volume_processed_data %>% 
  filter(day_of_week != "Sunday") %>% 
  filter(day_of_week != "Saturday") %>% 
  mutate(year = year(full_date)) %>% 
  filter(!is.na(cars_per_hour)) %>% 
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ))






# Training data are not 2020
pike_volume_train <- pike_volume_data %>% 
  filter(year != 2020)


# Testing data are 2020
pike_volume_test <- pike_volume_data %>% 
  filter(year == 2020) %>% 
  select(-year)





# Recipe
pike_recipe <- recipe(cars_per_hour ~ ., data = pike_volume_train) %>% 
  step_rm(full_date) %>% 
  step_dummy(c(day_of_week, direction, location), one_hot = TRUE) %>% 
  step_normalize(all_predictors())


# Prep Recipe
prep(pike_recipe) %>% 
  bake(new_data = NULL)


# Establishing Folds- keeping things manageable
pike_folds <- vfold_cv(pike_volume_train, v = 8, repeats = 5)

print("I am done with `pike_folds`")


# Boosted Tree Model
bt_model <- boost_tree(mode = "regression",
                       mtry = tune(),
                       min_n = tune(),
                       learn_rate = tune(),
                       trees = tune()
) %>%
  # importance will make a variable plot at the end
  set_engine("xgboost", importance = "impurity")


# Boosted Tree Workflow
bt_workflow <- workflow() %>%
  add_recipe(pike_recipe) %>%
  add_model(bt_model)



# Establishing Parameters
bt_params <- parameters(bt_model) %>%
  update(mtry = mtry(range = c(1, 7)),
         learn_rate = learn_rate(range = c(0.25, 0.75), trans = identity_trans())
  )





# Setting Up a Grid
bt_grid <- grid_regular(bt_params, levels = c(mtry = 7, min_n = 3, learn_rate = 4, trees = 8))

bt_grid


print("I am done with `bt_grid`")





# Boosted Tree Tuning Variables
bt_tuned <- bt_workflow %>%
  tune_grid(pike_folds, grid = bt_grid)


print("I am done with `bt_tuned`")


write_rds(bt_tuned, "bt_results.rds")



bt_workflow_tuned <- bt_workflow %>%
  finalize_workflow(select_best(bt_tuned, metric = "rmse"))

bt_tuned_train <- fit(bt_workflow_tuned, pike_volume_train)


write_rds(bt_tuned_train, "bt_tuned_train_results.rds")





# Boosted Trees Model- 2nd Attempt ----------------------------------------
# Reading in Data
pike_volume_processed_data <- read_rds("pike_volume_processed_data.rds")



# Filtering out Data
pike_volume_data <- pike_volume_processed_data %>% 
  filter(day_of_week != "Sunday") %>% 
  filter(day_of_week != "Saturday") %>% 
  mutate(year = year(full_date)) %>% 
  filter(!is.na(cars_per_hour)) %>% 
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ))






# Training data are not 2020
pike_volume_train <- pike_volume_data %>% 
  filter(year != 2020)


# Testing data are 2020
pike_volume_test <- pike_volume_data %>% 
  filter(year == 2020) %>% 
  select(-year)





# Recipe
pike_recipe <- recipe(cars_per_hour ~ ., data = pike_volume_train) %>% 
  step_rm(full_date) %>% 
  step_dummy(c(day_of_week, direction, location), one_hot = TRUE) %>% 
  step_normalize(all_predictors())


# Prep Recipe
prep(pike_recipe) %>% 
  bake(new_data = NULL)


# Establishing Folds- keeping things manageable
pike_folds <- vfold_cv(pike_volume_train, v = 8, repeats = 5)

print("I am done with `pike_folds`")


# Boosted Tree Model
bt_model <- boost_tree(mode = "regression",
                       mtry = tune(),
                       min_n = tune(),
                       learn_rate = tune(),
                       trees = tune()
) %>%
  # importance will make a variable plot at the end
  set_engine("xgboost", importance = "impurity")


# Boosted Tree Workflow
bt_workflow <- workflow() %>%
  add_recipe(pike_recipe) %>%
  add_model(bt_model)



# Establishing Parameters
bt_params <- parameters(bt_model) %>%
  update(mtry = mtry(range = c(1, 7)),
         learn_rate = learn_rate(range = c(0.25, 0.75), trans = identity_trans())
  )


# Setting Up a Grid
bt_grid <- grid_regular(bt_params, levels = c(mtry = 7, min_n = 3, learn_rate = 4, trees = 8))


print("I am done with `bt_grid`")



# Boosted Tree Tuning Variables
bt_tuned <- bt_workflow %>%
  tune_grid(pike_folds, grid = bt_grid)


print("I am done with `bt_tuned`")


write_rds(bt_tuned, "bt_results_adjusted.rds")


bt_workflow_tuned <- bt_workflow %>%
  finalize_workflow(select_best(bt_tuned, metric = "rmse"))

bt_tuned_train <- fit(bt_workflow_tuned, pike_volume_train)


write_rds(bt_tuned_train, "bt_tuned_train_results_adjusted.rds")





# Nearest Neighbor Model- 1st Attempt -------------------------------------
# Reading in Data
pike_volume_processed_data <- read_rds("pike_volume_processed_data.rds")



# Filtering out Data
pike_volume_data <- pike_volume_processed_data %>% 
  filter(day_of_week != "Sunday") %>% 
  filter(day_of_week != "Saturday") %>% 
  mutate(year = year(full_date)) %>% 
  filter(!is.na(cars_per_hour)) %>% 
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ))






# Training data are not 2020
pike_volume_train <- pike_volume_data %>%
  filter(year != 2020)


# Testing data are 2020
pike_volume_test <- pike_volume_data %>% 
  filter(year == 2020) %>% 
  select(-year)

dim(pike_volume_train)


# Recipe
pike_recipe <- recipe(cars_per_hour ~ ., data = pike_volume_train) %>% 
  step_rm(full_date) %>% 
  step_dummy(c(day_of_week, direction, location)) %>% 
  step_normalize(all_predictors())


# Prep Recipe
prep(pike_recipe) %>% 
  bake(new_data = NULL)


# Establishing Folds- keeping things manageable
pike_folds <- vfold_cv(pike_volume_train, v = 6, repeats = 3)



# Nearest Neighbor Model
nn_model <- nearest_neighbor(mode = "regression", neighbors = tune()) %>% 
  set_engine("kknn")


# Nearest Neighbor Workflow
nn_workflow <- workflow() %>%
  add_recipe(pike_recipe) %>%
  add_model(nn_model)



# Establishing Parameters
nn_params <- parameters(nn_model) %>%
  update(neighbors = neighbors(range = c(1, 15)))



# Setting Up a Grid
nn_grid <- grid_regular(nn_params, levels = 10)



# Nearest Neighbor Tuning Variables
nn_tuned <- nn_workflow %>%
  tune_grid(resamples = pike_folds, grid = nn_grid)




write_rds(nn_tuned, "nn_results_reduced.rds")

print("I am done with `nn_tuned`")



nn_workflow_tuned <- nn_workflow %>%
  finalize_workflow(select_best(nn_tuned, metric = "rmse"))

nn_tuned_train <- fit(nn_workflow_tuned, pike_volume_train)


write_rds(nn_tuned_train, "nn_tuned_train_results_reduced.rds")





# Nearest Neighbor Model- 2nd Attempt -------------------------------------
# Reading in Data
pike_volume_processed_data <- read_rds("pike_volume_processed_data.rds")



# Filtering out Data
pike_volume_data <- pike_volume_processed_data %>% 
  filter(day_of_week != "Sunday") %>% 
  filter(day_of_week != "Saturday") %>% 
  mutate(year = year(full_date)) %>% 
  filter(!is.na(cars_per_hour)) %>% 
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ))






# Training data are not 2020
pike_volume_train <- pike_volume_data %>%
  filter(year != 2020)


# Testing data are 2020
pike_volume_test <- pike_volume_data %>% 
  filter(year == 2020) %>% 
  select(-year)

dim(pike_volume_train)


# Recipe
pike_recipe <- recipe(cars_per_hour ~ ., data = pike_volume_train) %>% 
  step_rm(full_date) %>% 
  step_dummy(c(day_of_week, direction, location)) %>% 
  step_normalize(all_predictors())


# Prep Recipe
prep(pike_recipe) %>% 
  bake(new_data = NULL)


# Establishing Folds- keeping things manageable
pike_folds <- vfold_cv(pike_volume_train, v = 10, repeats = 5)



# Nearest Neighbor Model
nn_model <- nearest_neighbor(mode = "regression", 
                             neighbors = tune(), 
                             weight_func = tune(),
                             dist_power = tune()
) %>% 
  set_engine("kknn")


# Nearest Neighbor Workflow
nn_workflow <- workflow() %>%
  add_recipe(pike_recipe) %>%
  add_model(nn_model)



# Establishing Parameters
nn_params <- parameters(nn_model) %>%
  update(neighbors = neighbors(range = c(1, 15)))


# Setting Up a Grid
nn_grid <- grid_regular(nn_params, levels = c(neighbors = 13, weight_func = 5, dist_power = 5))


# Nearest Neighbor Tuning Variables
nn_tuned <- nn_workflow %>%
  tune_grid(resamples = pike_folds, grid = nn_grid)




write_rds(nn_tuned, "nn_results_adjusted.rds")

print("I am done with `nn_tuned`")



nn_workflow_tuned <- nn_workflow %>%
  finalize_workflow(select_best(nn_tuned, metric = "rmse"))

nn_tuned_train <- fit(nn_workflow_tuned, pike_volume_train)


write_rds(nn_tuned_train, "nn_tuned_train_results_adjusted.rds")





# Random Forest Model -----------------------------------------------------
# Reading in Data
pike_volume_processed_data <- read_rds("pike_volume_processed_data.rds")



# Filtering out Data
pike_volume_data <- pike_volume_processed_data %>% 
  filter(day_of_week != "Sunday") %>% 
  filter(day_of_week != "Saturday") %>% 
  mutate(year = year(full_date)) %>% 
  filter(!is.na(cars_per_hour)) %>% 
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ))






# Training data are the month of march
pike_volume_train <- pike_volume_data %>% 
  filter(year != 2020) %>% 
  select(-c(year))



# Testing data are march 2020
pike_volume_test <- pike_volume_data %>% 
  filter(year == 2020) %>% 
  select(-c(year, month))




# Recipe
pike_recipe <- recipe(cars_per_hour ~ ., data = pike_volume_train) %>% 
  step_rm(full_date) %>% 
  step_dummy(c(day_of_week, direction, location)) %>% 
  step_normalize(all_predictors())


# Prep Recipe
prep(pike_recipe) %>% 
  bake(new_data = NULL)



# Establishing Folds
pike_folds <- vfold_cv(pike_volume_train, v = 5, repeats = 2)


print("I am done with `pike_folds`")



# Random Forest Model
rf_model <- rand_forest(
  mode = "regression",
  min_n = tune(),
  mtry = tune()
) %>% 
  set_engine("ranger")


# Random Forest Workflow
rf_workflow <- workflow() %>% 
  add_recipe(pike_recipe) %>% 
  add_model(rf_model)


# Establishing parameters
rf_params <- parameters(rf_model) %>% 
  # There are 7 other columns using to predict things
  update(mtry = mtry(c(1, 7)))


# Setting Up a Grid
rf_grid <- grid_regular(rf_params, levels = 3)

print("I am done with `rf_grid`")


# Random Forest Tuning Variables
rf_tuned <- rf_workflow %>% 
  tune_grid(pike_folds, grid = rf_grid)

write_rds(rf_tuned, "rf_results.rds")





# Best Model Fit ----------------------------------------------------------
# Reading in Data
pike_volume_processed_data <- read_rds("pike_volume_processed_data.rds")



# Filtering out Data
pike_volume_data <- pike_volume_processed_data %>% 
  filter(day_of_week != "Sunday") %>% 
  filter(day_of_week != "Saturday") %>% 
  mutate(year = year(full_date)) %>% 
  filter(!is.na(cars_per_hour)) %>% 
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ))






# Training data are not 2020
pike_volume_train <- pike_volume_data %>% 
  filter(year != 2020) %>% 
  select(-c(year))



# Testing data are 2020
pike_volume_test <- pike_volume_data %>% 
  filter(year == 2020) %>% 
  select(-c(year))




# Recipe
pike_recipe <- recipe(cars_per_hour ~ ., data = pike_volume_train) %>% 
  step_rm(full_date) %>% 
  step_dummy(c(day_of_week, direction, location)) %>% 
  step_normalize(all_predictors())


# Boosted Tree Model
best_model <- boost_tree(mode = "regression",
                         mtry = tune(),
                         min_n = tune(),
                         learn_rate = tune(),
                         trees = tune()
) %>%
  # importance will make a variable plot at the end
  set_engine("xgboost")


# Boosted Tree Workflow
best_workflow <- workflow() %>%
  add_recipe(pike_recipe) %>%
  add_model(best_model)




# Prep Recipe
prep(pike_recipe) %>% 
  bake(new_data = NULL)


best_tuned <- read_rds("bt_results_adjusted.rds")





# My best model was the boosted trees
best_workflow_tuned <- best_workflow %>% 
  finalize_workflow(select_best(best_tuned, metric = "rmse"))

best_results <- fit(best_workflow_tuned, pike_volume_train)



pike_metric <- metric_set(rmse)


best_model_predictions <- predict(best_results, new_data = pike_volume_test) %>% 
  bind_cols(pike_volume_test %>% select(cars_per_hour)) %>% 
  pike_metric(truth = cars_per_hour, estimate = .pred) 



write_rds(best_model_predictions, "best_model_predictions.rds")



predict(best_results, new_data = pike_volume_test) %>% 
  bind_cols(pike_volume_test %>% select(cars_per_hour)) %>% 
  ggplot(aes(x = cars_per_hour, y = .pred)) +
  geom_point(alpha = 1/5, color = "steelblue") +
  labs(
    title = "Plot of the Best Model (Boosted Trees)",
    x = "Cars per Hour",
    y = "Differing Predicted Values"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(family = "serif", size = 9),
    axis.title = element_text(family = "serif", size = 9),
    axis.text.x = element_text(size = 6),
    axis.ticks = element_line(size = 2, color = "grey80"),
    panel.grid.major = element_line(size = 1)
  ) +
  geom_abline() +
  ggsave("best_model_results.png")