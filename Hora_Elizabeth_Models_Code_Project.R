# Loading Packages ----
library(tidyverse)
library(gganimate)
library(broom)
library(readxl)
library(lubridate)
library(lvplot)
library(janitor)
library(parsnip)
library(tidymodels)
library(corrplot)




# Geography ----
# Part 1
ma_counties <- ggplot2::map_data("county", "massachusetts") %>% 
  dplyr::select(lon = long, lat = lat, group, id = subregion)


# Part 2
ma_important_places <- tribble(
  ~city, ~latitude, ~longitude,
  "Framingham", 42.309515, -71.436233,
  "Cochituate", 42.331647, -71.322605,
  "Newton", 42.350590, -71.211140,
  "Pre Alston Interchange", 42.357814, -71.153235,
  "Post Alston Interchange", 42.352682, -71.115337
  
)

# Part 3
ggplot(ma_counties, aes(lon, lat)) +
  geom_polygon(aes(group = group), fill = "white", colour = "grey50") +
  geom_point(data = ma_important_places, mapping = aes(x = longitude, y = latitude), color = "RED") +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "Counties in Massachusetts",
    subtitle = "Marking Selected Locations Along the Mass Pike"
  )




# Reading in the Data ----
pike_class_processed_data <- read_rds("processed_rds_files/pike_class_processed_data.rds")
pike_speed_processed_data <- read_rds("processed_rds_files/pike_speed_processed_data.rds")
pike_volume_processed_data <- read_rds("processed_rds_files/pike_volume_processed_data.rds")




# Complete Data ----
# Class
# Collecting Information
pike_class_available <- pike_class_processed_data %>% 
  mutate(year = as.factor(year(full_date))) %>% 
  mutate(month = as.factor(month(full_date))) %>% 
  filter(total != 0) %>% 
  group_by(year, location, direction) %>% 
  count() 

# Plotting
ggplot(pike_class_available, aes(x = year, y = n, fill = location)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Number of Recorded Days per Location",
    y = "Number of Days",
    x = "Year"
  )




# Speed
# Collecting Information
pike_speed_available <- pike_speed_processed_data %>% 
  mutate(year = as.factor(year(full_date))) %>% 
  mutate(month = as.factor(month(full_date))) %>% 
  filter(total != 0) %>% 
  group_by(year, location, direction) %>% 
  count()

# Plotting
ggplot(pike_speed_available, aes(x = year, y = n, fill = location)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Number of Recorded Days per Location",
    y = "Number of Days",
    x = "Year"
  )




# Volume
# Collecting Information
pike_volume_available <- pike_volume_processed_data %>% 
  group_by(year, location, direction) %>% 
  filter(!is.na(cars_per_hour)) %>% 
  count() %>% 
  mutate(n = n / 24)

# Plotting 
ggplot(pike_volume_available, aes(x = year, y = n, fill = location)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Number of Recorded Days per Location",
    y = "Number of Days",
    x = "Year"
  )





# Splitting the Data ----
# Wrangling Volume Data
pike_volume_data <- pike_volume_processed_data %>% 
  filter(day_of_week != "Sunday") %>% 
  filter(day_of_week != "Saturday") %>% 
  mutate(year = year(full_date)) %>% 
  filter(!is.na(cars_per_hour)) %>% 
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ))

# Wrangling Class and Speed Data
pike_class_data <- read_rds("processed_rds_files/pike_class_processed_data.rds") %>% 
  mutate(day_of_week = wday(full_date, label = TRUE, abbr = FALSE)) %>% 
  filter(day_of_week != "Sunday") %>% 
  filter(day_of_week != "Saturday") %>% 
  mutate(year = year(full_date)) %>% 
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  )) %>% 
  filter(total != 0)



pike_speed_data <- read_rds("processed_rds_files/pike_speed_processed_data.rds") %>% 
  mutate(day_of_week = wday(full_date, label = TRUE, abbr = FALSE)) %>% 
  filter(day_of_week != "Sunday") %>% 
  filter(day_of_week != "Saturday") %>% 
  mutate(year = year(full_date)) %>% 
  mutate(month = month(full_date)) %>% 
  mutate(day = day(full_date)) %>% 
  mutate(day_of_week = factor(day_of_week, 
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  )) %>% 
  filter(total != 0)

pike_class_speed_data <- left_join(
  pike_class_data,
  pike_speed_data,
  by = c("direction", "location", "day_of_week", "year", "full_date", "total")
)


pike_class_speed_data_avg_speed <- pike_class_speed_data %>% 
  group_by(total, direction, location, year, month, day) %>% 
  mutate(x0_20_mph_wt = x0_20_mph/total,
         x20_25_mph_wt = x20_25_mph/total * 25,
         x25_30_mph_wt = x25_30_mph/total * 30,
         x30_35_mph_wt = x30_35_mph/total * 35,
         x35_40_mph_wt = x35_40_mph/total * 40,
         x40_45_mph_wt = x40_45_mph/total * 45,
         x45_50_mph_wt = x45_50_mph/total * 50,
         x50_55_mph_wt = x50_55_mph/total * 55,
         x55_60_mph_wt = x55_60_mph/total * 60,
         x60_65_mph_wt = x60_65_mph/total * 65,
         x65_70_mph_wt = x65_70_mph/total * 70,
         x70_mph_wt = x70_mph/total * 75
  ) %>%
  select(contains("wt")) %>% 
  mutate(daily_sum = sum(c(x0_20_mph_wt, x20_25_mph_wt,
                           x25_30_mph_wt, x30_35_mph_wt,
                           x35_40_mph_wt, x40_45_mph_wt,
                           x45_50_mph_wt, x50_55_mph_wt,
                           x55_60_mph_wt, x60_65_mph_wt,
                           x65_70_mph_wt, x70_mph_wt))) %>% 
  select(-contains("wt"))


pike_class_speed_joined <- left_join(
  pike_class_speed_data_avg_speed,
  pike_class_speed_data,
  by = c("direction", "location", "year", "total", "day" = "date", "month")
) %>% 
  select(-day.y)



# Training Set for Volume
pike_volume_train <- pike_volume_data %>% 
  filter(year != 2020) %>% 
  select(-c(year))

# Testing Set for Volume
pike_volume_test <- pike_volume_data %>% 
  filter(year == 2020) %>% 
  select(-c(year))

# Training Set for Class and Speed
pike_class_speed_train <- pike_class_speed_joined %>% 
  filter(year != 2020)

# Testing Set for Class and Speed
pike_class_speed_test <- pike_class_speed_joined %>% 
  filter(year == 2020)




# Looking at Model Results for Volume ----
# Linear Model
# Pike recipe
pike_recipe_linear <- recipe(cars_per_hour ~ ., data = pike_volume_train) %>% 
  step_rm(full_date) %>% 
  step_dummy(c(day_of_week, direction, location)) %>% 
  step_ns(day, hour, month)


# Linear Model
linear_model <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("lm") 


# Test Workflow
pike_linear_workflow <- workflow() %>% 
  add_model(linear_model) %>% 
  add_recipe(pike_recipe_linear)

# Fitting the results
test_results <- fit(pike_linear_workflow, pike_volume_train)

# Setting my rmse metric
linear_metric_set <- metric_set(rmse)

# Creating a prediction
predict(test_results, new_data = pike_volume_test) %>% 
  bind_cols(pike_volume_test %>% select(cars_per_hour)) %>% 
  linear_metric_set(truth = cars_per_hour, estimate = .pred)



# Random Forest
# Random forest
rf_tuned <- read_rds("model_rds_files/rf_results.rds")
autoplot(rf_tuned, metric = "rmse")



# Boosted Trees- 1st Attempt
bt_tuned <- read_rds("model_rds_files/bt_results.rds")
autoplot(bt_tuned, metric = "rmse")



# Boosted Trees- 2nd Attempt
bt_tuned_adj <- read_rds("model_rds_files/bt_results_adjusted.rds")
autoplot(bt_tuned_adj, metric = "rmse")



# Nearest Neighbor- 1st Attempt
nn_tuned <- read_rds("model_rds_files/nn_results.rds")
autoplot(nn_tuned, metric = "rmse") +
  labs(x = "# neighbors")



# Nearest Neighbor- 2nd Attempt
nn_tuned_reduced <- read_rds("model_rds_files/nn_results_reduced.rds")
autoplot(nn_tuned_reduced, metric = "rmse") +
  labs(x = "# neighbors")




# Comparing Model Results ----
show_best(rf_tuned, metric = "rmse")
show_best(bt_tuned, metric = "rmse")
show_best(bt_tuned_adj, metric = "rmse")
show_best(nn_tuned, metric = "rmse")
show_best(nn_tuned_reduced, metric = "rmse")




# Looking at Model Results for Class and Speed ----
# Plotting Fitted data
pike_class_speed_tuned <- read_rds("class_speed_work/bt_results_avg_speed.rds")
autoplot(pike_class_speed_tuned, metric = "rmse")

show_best(pike_class_speed_tuned, metric = "rmse")




# Looking at Model Prediction Results ----
# Volume
best_model <- read_rds("model_rds_files/best_model_predictions.rds")
best_model



# Class and Speed
bt_class_speed_predictions <- read_rds("model_rds_files/bt_model_predictions.rds")
bt_class_speed_predictions




# Test Data Reveal ----
# Yearly Average Speeds in January & February versus April versus November
# Examining Yearly Average Speeds in January and February
pike_class_speed_joined %>% 
  filter(!is.na(daily_sum)) %>% 
  filter(month == 1 | month == 2) %>% 
  group_by(c(year)) %>% 
  summarize(
    mean_daily_sum = mean(daily_sum),
    median_daily_sum = median(daily_sum)
  )


# Examining Yearly Average Speeds in April
pike_class_speed_joined %>% 
  filter(!is.na(daily_sum)) %>% 
  filter(month == 4) %>% 
  group_by(c(year)) %>% 
  summarize(
    mean_daily_sum = mean(daily_sum),
    median_daily_sum = median(daily_sum)
  )


# Examining Yearly Average Speeds in November
pike_class_speed_joined %>% 
  filter(!is.na(daily_sum)) %>% 
  filter(month == 11) %>% 
  group_by(c(year)) %>% 
  summarize(
    mean_daily_sum = mean(daily_sum),
    median_daily_sum = median(daily_sum)
  )




# Total Number of Vehicles in April and November in 2017 and 2020
# Examining Total Number of Cars in April
pike_class_speed_joined %>% 
  filter(month == 4 & year == 2017) %>% 
  select(total)


pike_class_speed_joined %>% 
  filter(month == 4 & year == 2020) %>% 
  select(total)


# Examining Yearly Average Speeds in November
pike_class_speed_joined %>% 
  filter(month == 11 & year == 2017) %>% 
  select(total)


pike_class_speed_joined %>% 
  filter(month == 11 & year == 2020) %>% 
  select(total)