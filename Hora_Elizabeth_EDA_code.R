
# Final Project -----------------------------------------------------------
# Loading Packages ----
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)
library(naniar)
library(visdat)
library(lubridate)
library(stringr)
library(gganimate)
library(lvplot)




# Initial Data Issues ----
elm_1_1997_raw <- read_excel("data/elm_street/MonthlyVolumeReport_403_WB_1_1997.xlsx")
elm_1_1997_raw


times <- as.character(0:23)
elm_8_2018_skipped <- read_excel("data/elm_street/MonthlyVolumeReport_403_WB_8_2018.xlsx", skip = 10, col_names = c("day", times, "Total", "QC Status"))
elm_8_2018_skipped




# Solving the Data Issues for the Concord Rotary ----
# Step 1
# Part 1
files <- dir("data/elm_street/", pattern = "\\.xlsx$", full.names = TRUE)

# Part 2
file_list <- list()


# Step 2
# Part 1
times <- as.character(0:23)

# Part 2
for (i in files){
  file_list[[i]] <- read_excel(i, skip = 10, col_names = c("day", times, "Total", "QC Status")) %>% 
    pivot_longer(c(`0`,`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`), 
                 names_to = "hour", values_to = "cars_per_hour") %>% 
    select("day", "hour", "cars_per_hour")
}


# Step 3
# Part 1
months <- str_extract(names(file_list), "_403_[A-Z]B_\\d+") %>% 
  str_sub(9) %>% 
  as.double()

# Part 2
years <- str_extract(names(file_list), "\\d{4}") %>% 
  as.double()

# Part 3
travel_direction <- str_extract(names(file_list), "_403_[A-Z]B") %>% 
  str_sub(6)


# Step 4
for (i in 1:length(files)){
  file_list[[i]] <- file_list[[i]] %>% 
    mutate(hour = as.numeric(hour)) %>%  
    mutate(full_date = make_datetime(year = years[i], month = months[i], day = day, hour = hour)) %>% 
    mutate(day_of_week = wday(full_date, label = TRUE, abbr = FALSE)) %>% 
    mutate(direction = travel_direction[i])
}


# Step 5
elm_master <- bind_rows(file_list)
elm_master




# Solving the Data Issues for the Mass Pike Locations ----
# Step 1
# Part 1
paths_to_open <- c("data/mass_pike/framingham", 
                   "data/mass_pike/cochituate", 
                   "data/mass_pike/newton",
                   "data/mass_pike/pre_allston",
                   "data/mass_pike/post_allston"
)

# Part 2
files <- dir(paths_to_open, pattern = "\\.xlsx$", full.names = TRUE)

# Part 3
pike_file_list <- list()


# Step 2
for (i in files){
  pike_file_list[[i]] <- read_excel(i, skip = 10, col_names = c("day", times, "Total", "QC Status")) %>% 
    pivot_longer(c(`0`,`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`), 
                 names_to = "hour", values_to = "cars_per_hour") %>% 
    select("day", "hour", "cars_per_hour", "Total")
} 


# Step 3
# Part 1
months <- str_extract(names(pike_file_list), "_[A-Z]B_\\d+") %>% 
  str_sub(start = 5) %>% 
  as.double()

# Part 2
years <- str_extract(names(pike_file_list), "\\d{4}") %>% 
  as.double()

# Part 3
travel_direction <- str_extract(names(pike_file_list), "_[A-Z]B_") %>% 
  str_sub(start = 2L, end = -2L) 

# Part 4
location <- str_extract(names(pike_file_list), "/mass_pike/..") %>% 
  str_sub(start = 12L) %>% 
  as.character()


# Step 4
for (i in 1:length(files)){
  pike_file_list[[i]] <- pike_file_list[[i]] %>% 
    mutate(hour = as.numeric(hour)) %>%  
    mutate(full_date = make_datetime(year = years[i], month = months[i], day = day, hour = hour)) %>% 
    mutate(day_of_week = wday(full_date, label = TRUE, abbr = FALSE)) %>% 
    mutate(direction = travel_direction[i]) %>% 
    mutate(location = location[i])
}


# Step 5
# Part 1
pike_master <- bind_rows(pike_file_list)

# Part 2
pike_master <- pike_master %>% 
  mutate(daily_percent = cars_per_hour / Total * 100)




# Weekends vs Weekdays ----
# East Bound- Concord Rotary
elm_master %>%
  filter(direction == "EB") %>% 
  mutate(day_of_week = fct_relevel(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  mutate(year = as.factor(year(full_date))) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  facet_grid(year~day_of_week)


# West Bound- Concord Rotary
elm_master %>% 
  filter(direction == "WB") %>% 
  mutate(day_of_week = fct_relevel(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  mutate(year = as.factor(year(full_date))) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  facet_grid(year~day_of_week)


# Panel View- Concord Rotary
for (i in 1:length(files)){
  file_list[[i]] %>% 
    ggplot() +
    aes(x = hour, y = cars_per_hour, color = day_of_week) +
    geom_point() + 
    geom_smooth(se = FALSE)  
  ggsave(filename = paste(years[i], 
                          "_",
                          months[i], 
                          sep = "",
                          device = ".png"
                          ),
         path = "rotary_panel",
         )
}


# East Bound- Mass Pike
pike_master %>% 
  filter(direction == "EB") %>% 
  mutate(day_of_week = fct_relevel(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  mutate(year = as.factor(year(full_date))) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  facet_grid(year~day_of_week)


# West Bound- Mass Pike
pike_master %>% 
  filter(direction == "WB") %>% 
  mutate(day_of_week = fct_relevel(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  mutate(year = as.factor(year(full_date))) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  facet_grid(year~day_of_week)


# Filtering out the Weekends for Both Sets ----
# Concord Rotary Without Weekends
elm_master_EB <- elm_master %>% 
  filter(direction == "EB") %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday")

elm_master_WB <- elm_master %>% 
  filter(direction == "WB") %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday")


# Mass Pike Without Weekends
pike_master_EB <- pike_master %>% 
  drop_na() %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday") %>% 
  filter(direction == "EB") %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po"))) 

pike_master_WB <- pike_master %>% 
  drop_na() %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday") %>% 
  filter(direction == "WB") %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po")))




# Flux ----
# Concord Rotary
# Part 1
elm_master_EB_flux <- elm_master_EB %>% 
  select(day, hour, cars_per_hour, day_of_week, direction, full_date) 
elm_master_WB_flux <- elm_master_WB %>% 
  select(full_date, day, hour, cars_per_hour, day_of_week, direction)

# Part 2
elm_master_flux_per_hour <- 
  left_join(elm_master_EB_flux, elm_master_WB_flux, by = c("full_date", "hour")) %>% 
  mutate(day = day.x) %>% 
  mutate(month = month(full_date)) %>% 
  mutate(year = year(full_date)) %>% 
  mutate(cars_per_hour_EB = cars_per_hour.x) %>% 
  mutate(cars_per_hour_WB = cars_per_hour.y) %>% 
  mutate(day_of_week = day_of_week.x) %>% 
  mutate(flux = (cars_per_hour_EB - cars_per_hour_WB)) %>% 
  select(full_date, day, hour, month, year, cars_per_hour_EB, cars_per_hour_WB, day_of_week, flux)


# Plotting
ggplot(data = elm_master_flux_per_hour, mapping = aes(x = hour, y = flux, color = year)) +
  geom_point() +
  geom_smooth(color = "RED") +
  facet_wrap(~year, nrow = 2)


# Mass Pike
# Part 1
pike_master_EB_flux <- pike_master_EB %>% 
  select(day, hour, cars_per_hour, day_of_week, direction, location, full_date)
pike_master_WB_flux <- pike_master_WB %>% 
  select(full_date, day, hour, cars_per_hour, day_of_week, direction, location)

# Part 2
pike_master_flux_per_hour <- 
  left_join(pike_master_EB_flux, pike_master_WB_flux, by = c("full_date", "location", "hour")) %>% 
  mutate(day = day.x) %>% 
  mutate(month = month(full_date)) %>% 
  mutate(year = year(full_date)) %>% 
  mutate(cars_per_hour_EB = cars_per_hour.x) %>% 
  mutate(cars_per_hour_WB = cars_per_hour.y) %>% 
  mutate(day_of_week = day_of_week.x) %>% 
  mutate(flux = (cars_per_hour_EB - cars_per_hour_WB)) %>% 
  select(full_date, day, hour, month, year, cars_per_hour_EB, cars_per_hour_WB, day_of_week, location, flux)

# Plotting AM Hours
pike_master_flux_per_hour %>% 
  filter(hour < 12) %>% 
  ggplot(aes(x = location, y = flux, color = location)) +
  geom_lv() +
  facet_wrap(~hour, nrow = 2)


# Plotting PM Hours
pike_master_flux_per_hour %>% 
  filter(hour >= 12) %>% 
  ggplot(aes(x = location, y = flux, color = location)) +
  geom_lv() +
  facet_wrap(~hour, nrow = 2)


# Flux Overall
# Part 1
pike_master_EB_11_hour <- pike_master_EB %>% 
  filter(hour == 11) %>% 
  select(day, hour, Total, day_of_week, direction, location, full_date)
pike_master_WB_11_hour <- pike_master_WB %>% 
  filter(hour == 11) %>% 
  select(full_date, day, hour, Total, day_of_week, direction, location)

# Part 2
pike_master_EB_11_hour_location <- pike_master_EB_11_hour %>% 
  group_by(location)
pike_master_WB_11_hour_location <- pike_master_WB_11_hour %>% 
  group_by(location)

# Part 3
pike_master_flux_overall <- 
  left_join(pike_master_EB_11_hour_location, pike_master_WB_11_hour_location, by = c("full_date", "location")) %>% 
  mutate(day = day.x) %>% 
  mutate(month = month(full_date)) %>% 
  mutate(hour = hour.x) %>% 
  mutate(year = year(full_date)) %>% 
  mutate(Total_EB = Total.x) %>% 
  mutate(Total_WB = Total.y) %>% 
  mutate(day_of_week = day_of_week.x) %>% 
  mutate(flux = (Total_EB - Total_WB)) %>% 
  mutate(full_date = as.character(full_date)) %>% 
  select(full_date, day, hour, month, year, Total_EB, Total_WB, day_of_week, location, flux)


# Plotting Overall Flux
ggplot(pike_master_flux_overall, aes(x = location, y = flux, color = location)) +
  geom_lv() +
  geom_point()




# `gganimate` ----
# Part 1
elm_master_EB_wkd_with_year <- elm_master_EB %>% 
  mutate(year = year(full_date))

# Part 2
elm_master_EB_wkd_with_year_anim <- ggplot(elm_master_EB_wkd_with_year) +
  geom_point(aes(x = hour, y = cars_per_hour, color = year)) +
  transition_states(year,
                    transition_length = 2,
                    state_length = 1)
# Part 3
animate(elm_master_EB_wkd_with_year_anim, 
        renderer = file_renderer(dir = "data/elm_street/gganimate_EB_wkd", prefix = "gganim_frame", overwrite = FALSE)
)




# Mass Pike Congestion Visualization ----
# Weekdays
# East Bound
pike_master_EB %>% 
  filter(day_of_week == "Monday") %>% 
  group_by(day_of_week) %>% 
  group_map(~ggplot(., aes(x = hour, y = location)) +
              geom_tile(aes(fill = daily_percent)) +
              scale_fill_gradient(low = "white", high = "black") +
              labs(title = "EB Monday"))


# West Bound
pike_master_WB %>% 
  filter(day_of_week == "Monday") %>% 
  group_by(day_of_week) %>% 
  group_map(~ggplot(., aes(x = hour, y = location)) +
              geom_tile(aes(fill = daily_percent)) +
              scale_fill_gradient(low = "white", high = "black") +
              labs(title = "WB Monday"))


# Months
# East Bound
pike_master_EB %>% 
  mutate(month = month(full_date)) %>%
  filter(month == 3) %>% 
  group_by(month) %>% 
  group_map(~ggplot(., aes(x = hour, y = location)) +
              geom_tile(aes(fill = daily_percent)) +
              scale_fill_gradient(low = "white", high = "black", limits = c(0, 11)) +
              labs(title = "EB March"))


# West Bound
pike_master_WB %>% 
  mutate(month = month(full_date)) %>%
  filter(month == 3) %>% 
  group_by(month) %>% 
  group_map(~ggplot(., aes(x = hour, y = location)) +
              geom_tile(aes(fill = daily_percent)) +
              scale_fill_gradient(low = "white", high = "black", limits = c(0, 11)) +
              labs(title = "WB March"))




# Before and After Electronic Tolls on the Mass Pike ----
# Setting up the Next Code Chunk
pike_master_flux_day <- 
  left_join(pike_master_EB_11_hour_location, pike_master_WB_11_hour_location, by = c("full_date", "location")) %>% 
  mutate(day = day.x) %>% 
  mutate(month = month(full_date)) %>% 
  mutate(hour = hour.x) %>% 
  mutate(year = year(full_date)) %>% 
  mutate(Total_EB = Total.x) %>% 
  mutate(Total_WB = Total.y) %>% 
  mutate(day_of_week = day_of_week.x) %>% 
  mutate(flux = (Total_EB - Total_WB)) %>% 
  mutate(full_date = as.character(full_date)) %>% 
  select(full_date, day, hour, month, year, Total_EB, Total_WB, day_of_week, location, flux)

# Building Datasets for `2017` and `2019`
# 2017
# Part 1
pike_master_flux_day_2017 <- pike_master_flux_day %>% 
  filter((full_date >= ymd(20170101)) & (full_date <= ymd(20171231))) %>% 
  mutate(pre_year_total = sum(Total_EB + Total_WB)) %>% 
  count(pre_year_total) %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po")))

# Part 2
tibble_2017 <- tibble(
  location = c("fr", "co", "ne", "pr", "po"),
  time = "2017"
) %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po")))

# Part 3
pike_master_flux_day_2017 <- 
  left_join(pike_master_flux_day_2017, tibble_2017, by = "location")


# 2019
# Part 1
pike_master_flux_day_2019 <- pike_master_flux_day %>% 
  filter((full_date >= ymd(20190101)) & (full_date <= ymd(20191231))) %>% 
  mutate(post_year_total = sum(Total_EB + Total_WB)) %>% 
  count(post_year_total) %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po")))

# Part 2
tibble_2019 <- tibble(
  location = c("fr", "co", "ne", "pr", "po"),
  time = "2019"
) %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po")))

# Part 3
pike_master_flux_day_2019 <- 
  left_join(pike_master_flux_day_2019, tibble_2019, by = "location") 


# Combining `2017` and `2019`
# Part 1
pike_master_flux_2017_2019_toll <- bind_rows(pike_master_flux_day_2017, pike_master_flux_day_2019) 

# Part 2
pike_master_flux_2017_2019_toll[is.na(pike_master_flux_2017_2019_toll)] <- 0

# Part 3
pike_master_flux_2017_2019_toll <- pike_master_flux_2017_2019_toll %>% 
  mutate(year_total = pre_year_total + post_year_total) %>% 
  mutate(daily_average_both_directions = year_total / n) %>% 
  mutate(time = as.factor(time)) %>% 
  select(location, n, year_total, time, daily_average_both_directions)


# Plotting Results
pike_master_flux_2017_2019_toll %>%  
  mutate(time = fct_relevel(time, c("2019", "2017"))) %>% 
  ggplot(aes(x = location, y = daily_average_both_directions, color = time)) +
  geom_point()




# Distribution of Total Cars per Day for Locations on the Mass Pike
pike_master_flux_day_distribution <- pike_master_flux_day %>% 
  mutate(total_trips = Total_EB + Total_WB) %>%
  mutate(total_trips_in_1000s = total_trips / 1000) %>% 
  mutate(year = (year(full_date)))


# Using a Histogram
ggplot(pike_master_flux_day_distribution, aes(x = total_trips_in_1000s)) +
  geom_histogram() +
  facet_grid(year~location) +
  labs(x = "Daily Trips, in Thousands", y = "Number of Days")


# Using Violin Plots
ggplot(pike_master_flux_day_distribution, aes(x = location, y = total_trips)) +
  geom_violin() +
  facet_wrap(~year, ncol = 2) +
  labs(y = "Total Number of Trips in Both Directions Combined per Day")




# Coronavirus Pandemic ----
# Cochituate in March
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "co",
         month(full_date) == 3
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# Cochituate in March of 2020
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "co",
         month(full_date) == 3,
         year(full_date) == 2020
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  facet_wrap(~year(full_date), ncol = 2)


# Cochituate in March- Before and After the State of Emergency
# Before
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "co",
         full_date > ymd(20200301) & full_date < ymd(20200318)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE)


# After
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "co",
         full_date >= ymd(20200318) & full_date <= ymd(20200331)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  coord_cartesian(ylim = c(0, 4000))


# Cochituate in April
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "co",
         month(full_date) == 4
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# Cochituate in May
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "co",
         month(full_date) == 5
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)




















# Supplemental Materials --------------------------------------------------
# Missing Data ----
# Concord Rotary
elm_master %>%
  mutate(year = year(full_date)) %>% 
  select(cars_per_hour) %>% 
  vis_miss()

elm_master %>% 
  mutate(month = month(full_date)) %>% 
  mutate(year = year(full_date)) %>% 
  select(cars_per_hour, year) %>% 
  gg_miss_var(year, show_pct = TRUE)


# Mass Pike
pike_master %>%
  mutate(year = year(full_date)) %>% 
  select(cars_per_hour) %>% 
  vis_miss()

pike_master %>% 
  mutate(month = month(full_date)) %>% 
  mutate(year = year(full_date)) %>% 
  select(cars_per_hour, year) %>% 
  gg_miss_var(year, show_pct = TRUE) %>% 
  print()





# Weekends vs Weekdays ----
# Concord Rotary
# Concord Rotary East Bound
elm_master_EB <- elm_master %>% 
  filter(direction == "EB")

# Plotting the Data
elm_master_EB %>% 
  mutate(day_of_week = fct_relevel(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  mutate(year = as.factor(year(full_date))) %>% 
  ggplot(aes(x = hour, y = cars_per_hour, color = full_date)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "RED") + 
  facet_wrap(~day_of_week, nrow = 2)


# Concord Rotary West Bound
elm_master_WB <- elm_master %>% 
  filter(direction == "WB")

# Plotting the Data
elm_master_WB %>% 
  mutate(day_of_week = fct_relevel(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  mutate(year = as.factor(year(full_date))) %>% 
  ggplot(aes(x = hour, y = cars_per_hour, color = full_date)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "RED") + 
  facet_wrap(~day_of_week, nrow = 2)


# Mass Pike
# Mass Pike East Bound
pike_master_EB_with_weekends <- pike_master %>% 
  drop_na() %>% 
  filter(direction == "EB") %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po"))) %>% 
  mutate(year = as.factor(year(full_date)))

# Plotting the Data
pike_master_EB_with_weekends %>% 
  mutate(day_of_week = fct_relevel(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  ggplot(aes(x = hour, y = cars_per_hour, color = year)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "RED") + 
  facet_grid(day_of_week~location)


# Mass Pike West Bound
pike_master_WB_with_weekends <- pike_master %>% 
  drop_na() %>% 
  filter(direction == "EB") %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po"))) %>% 
  mutate(year = as.factor(year(full_date)))

# Plotting the Data
pike_master_EB_with_weekends %>% 
  mutate(day_of_week = fct_relevel(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  ggplot(aes(x = hour, y = cars_per_hour, color = year)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "RED") + 
  facet_grid(day_of_week~location)




# Mass Pike Congestion Visualization ----
# By Month
# East Bound
pike_master_EB <- pike_master %>% 
  drop_na() %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday") %>% 
  filter(direction == "EB") %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po"))) %>% 
  mutate(year = as.factor(year(full_date)))

pike_master_EB %>% 
  mutate(month = month(full_date)) %>% 
  group_by(month) %>% 
  group_map(~ggplot(., aes(x = hour, y = location)) +
              geom_tile(aes(fill = daily_percent)) +
              scale_fill_gradient(low = "white", high = "black", limits = c(0, 11)) +
              labs(title = month(.$full_date)))


# West Bound
pike_master_WB <- pike_master %>% 
  drop_na() %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday") %>% 
  filter(direction == "WB") %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po"))) %>% 
  mutate(year = as.factor(year(full_date)))

pike_master_WB %>% 
  group_by(month(full_date)) %>% 
  group_map(~ggplot(., aes(x = hour, y = location)) +
              geom_tile(aes(fill = daily_percent)) +
              scale_fill_gradient(low = "white", high = "black", limits = c(0, 11)) +
              labs(title = month(.$full_date)))




# By Weekday
# East Bound
pike_master_EB %>% 
  group_by(day_of_week) %>% 
  group_map(~ggplot(., aes(x = hour, y = location)) +
              geom_tile(aes(fill = daily_percent)) +
              scale_fill_gradient(low = "white", high = "black", limits = c(0, 11)) +
              labs(title = (day(.$full_date - 1))))


# West Bound
pike_master_WB %>% 
  group_by(day_of_week) %>% 
  group_map(~ggplot(., aes(x = hour, y = location)) +
              geom_tile(aes(fill = daily_percent)) +
              scale_fill_gradient(low = "white", high = "black", limits = c(0, 11)) +
              labs(title = (day(.$full_date - 1))))




# Flux per Hour ----
pike_master_EB_flux_hour <- pike_master_EB %>% 
  mutate(full_date = as.character(full_date)) %>% 
  select(day, hour, cars_per_hour, day_of_week, direction, location, full_date)
pike_master_WB_flux_hour <- pike_master_WB %>% 
  mutate(full_date = as.character(full_date)) %>% 
  select(full_date, day, hour, cars_per_hour, day_of_week, direction, location)


pike_master_flux_per_hour <- 
  left_join(pike_master_EB_flux_hour, pike_master_WB_flux_hour, by = c("full_date", "location", "hour")) %>% 
  mutate(day = day.x) %>% 
  mutate(month = month(full_date)) %>% 
  mutate(year = year(full_date)) %>% 
  mutate(cars_per_hour_EB = cars_per_hour.x) %>% 
  mutate(cars_per_hour_WB = cars_per_hour.y) %>% 
  mutate(day_of_week = day_of_week.x) %>% 
  mutate(flux = (cars_per_hour_EB - cars_per_hour_WB)) %>% 
  select(full_date, day, hour, month, year, cars_per_hour_EB, cars_per_hour_WB, day_of_week, location, flux)


# Creating Violin Plots
ggplot(pike_master_flux_per_hour, aes(x = location, y = flux, color = location)) +
  geom_violin() +
  facet_wrap(~hour, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90))




# Before and After Electronic Tolls on the Mass Pike ----
# Setting up the Next Code Chunk
pike_master_EB_11_hour <- pike_master_EB %>% 
  filter(hour == 11) %>% 
  select(day, hour, Total, day_of_week, direction, location, full_date)
pike_master_WB_11_hour <- pike_master_WB %>% 
  filter(hour == 11) %>% 
  select(full_date, day, hour, Total, day_of_week, direction, location)


pike_master_EB_11_hour_location <- pike_master_EB_11_hour %>% 
  group_by(location)
pike_master_WB_11_hour_location <- pike_master_WB_11_hour %>% 
  group_by(location)


pike_master_flux_day <- 
  left_join(pike_master_EB_11_hour_location, pike_master_WB_11_hour_location, by = c("full_date", "location")) %>% 
  mutate(day = day.x) %>% 
  mutate(month = month(full_date)) %>% 
  mutate(hour = hour.x) %>% 
  mutate(year = year(full_date)) %>% 
  mutate(Total_EB = Total.x) %>% 
  mutate(Total_WB = Total.y) %>% 
  mutate(day_of_week = day_of_week.x) %>% 
  mutate(flux = (Total_EB - Total_WB)) %>% 
  mutate(full_date = as.character(full_date)) %>% 
  select(full_date, day, hour, month, year, Total_EB, Total_WB, day_of_week, location, flux)


# 2017
# Part 1
pike_master_flux_day_pre_tolls <- pike_master_flux_day %>% 
  filter(full_date <= ymd(20181028)) %>% 
  mutate(pre_year_total = sum(Total_EB + Total_WB)) %>% 
  count(pre_year_total)

# Part 2
pre_tibble <- tibble(
  location = c("fr", "co", "ne", "pr", "po"),
  time = "pre"
) %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po")))

# Part 3
pike_master_flux_day_pre_tolls <- 
  left_join(pike_master_flux_day_pre_tolls, pre_tibble, by = "location")


# 2019
# Part 1:
pike_master_flux_day_post_tolls <- pike_master_flux_day %>% 
  filter(full_date > ymd(20181028)) %>% 
  mutate(post_year_total = sum(Total_EB + Total_WB)) %>% 
  count(post_year_total)

# Part 2:
post_tibble <- tibble(
  location = c("fr", "co", "ne", "pr", "po"),
  time = "post"
) %>% 
  mutate(location = fct_relevel(location, c("fr", "co", "ne", "pr", "po")))

# Part 3:
pike_master_flux_day_post_tolls <- 
  left_join(pike_master_flux_day_post_tolls, post_tibble, by = "location")


# Combining `pre` and `post` Datasets in One Tibble
# Part 1
pike_master_flux_toll <- bind_rows(pike_master_flux_day_pre_tolls, pike_master_flux_day_post_tolls)

# Part 2
pike_master_flux_toll[is.na(pike_master_flux_toll)] <- 0

# Part 3
pike_master_flux_toll <- pike_master_flux_toll %>% 
  mutate(year_total = pre_year_total + post_year_total) %>% 
  mutate(daily_average_both_directions = year_total / n) %>% 
  select(location, n, year_total, time, daily_average_both_directions)


# Plottting Results ----
ggplot(pike_master_flux_toll, aes(x = location, y = daily_average_both_directions, color = time)) +
  geom_point()




# Looking at the Post Allston Interchange Location in More Detail ----
# Preparing to Find the Number of Days Above 75000 Trips in Both Directions per Day
# Part 1
threshold_75k_2017_WB <- pike_master %>% 
  filter(location == "po") %>%
  filter(hour == 11) %>% 
  filter(year(full_date) == 2017) %>% 
  filter(direction == "WB") %>% 
  mutate(days_over_75k = (Total > 75000)) %>% 
  drop_na()

# Part 2
as.integer(as.logical(threshold_75k_2017_WB$days_over_75k)) %>% 
  sum()


# Looking at Each `Year` and `Direction`
# 2017, East Bound
threshold_75k_2017_EB <- pike_master %>% 
  filter(location == "po") %>%
  filter(hour == 11) %>% 
  filter(year(full_date) == 2017) %>% 
  filter(direction == "EB") %>% 
  mutate(days_over_75k = (Total > 75000)) %>% 
  drop_na()

as.integer(as.logical(threshold_75k_2017_EB$days_over_75k)) %>% 
  sum()


# 2019, West Bound
threshold_75k_2019_WB <- pike_master %>% 
  filter(location == "po") %>%
  filter(hour == 11) %>% 
  filter(year(full_date) == 2019) %>% 
  filter(direction == "WB") %>% 
  mutate(days_over_75k = (Total > 75000)) %>% 
  drop_na()

as.integer(as.logical(threshold_75k_2019_WB$days_over_75k)) %>% 
  sum()


# 2019, East Bound
threshold_75k_2019_EB <- pike_master %>% 
  filter(location == "po") %>%
  filter(hour == 11) %>% 
  filter(year(full_date) == 2019) %>% 
  filter(direction == "EB") %>% 
  mutate(days_over_75k = (Total > 75000)) %>% 
  drop_na()

as.integer(as.logical(threshold_75k_2019_EB$days_over_75k)) %>% 
  sum()





# Coronavirus Pandemic (from saved images) ----
# images created using the following for loop
# Then the images were placed next to each other in PowerPoint
for (i in 1:length(files)){
  file_list[[i]] %>% 
    ggplot() +
    aes(x = hour, y = cars_per_hour, color = day_of_week) +
    geom_point() + 
    geom_smooth(se = FALSE)  
  ggsave(filename = paste(location[i], 
                          "_",
                          years[i], 
                          "_",
                          months[i], 
                          device = ".png", 
                          sep = ""
                          ),
         path = "coronavirus_pandemic"
         )
}






# Coronavirus Pandemic Visualized Through `ggplot` ----
# Framingham
# March
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "fr",
         month(full_date) == 3
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# Before and After State of Emergency Issued
# Before
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "fr",
         full_date > ymd(20200301) & full_date < ymd(20200318)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  coord_cartesian(ylim = c(0, 4000))


# After
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "fr",
         full_date >= ymd(20200318) & full_date <= ymd(20200331)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  coord_cartesian(ylim = c(0, 4000))


# April
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "fr",
         month(full_date) == 4
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# May
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "fr",
         month(full_date) == 5
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)




# Newton
# March
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "ne",
         month(full_date) == 3
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# Before and After State of Emergency Issued
# Before
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "ne",
         full_date > ymd(20200301) & full_date < ymd(20200318)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  coord_cartesian(ylim = c(0, 4000))


# After
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "ne",
         full_date >= ymd(20200318) & full_date <= ymd(20200331)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  coord_cartesian(ylim = c(0, 4000))


# April
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "ne",
         month(full_date) == 4
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# May
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "ne",
         month(full_date) == 5
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)



# Pre Allston Interchange
# March
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "pr",
         month(full_date) == 3
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# Before and After State of Emergency Issued
# Before
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "pr",
         full_date > ymd(20200301) & full_date < ymd(20200318)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  coord_cartesian(ylim = c(0, 4000))



# After
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "pr",
         full_date >= ymd(20200318) & full_date <= ymd(20200331)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  coord_cartesian(ylim = c(0, 4000))


# April
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "pr",
         month(full_date) == 4
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# May
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "pr",
         month(full_date) == 5
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)



# Post Allston Interchange
# March
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "po",
         month(full_date) == 3
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# Before and After State of Emergency Issued
# Before
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "po",
         full_date > ymd(20200301) & full_date < ymd(20200318)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  coord_cartesian(ylim = c(0, 4400))


# After
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "po",
         full_date >= ymd(20200318) & full_date <= ymd(20200331)
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) + 
  coord_cartesian(ylim = c(0, 4400))


# April
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "po",
         month(full_date) == 4
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)


# May
pike_master %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday",
         location == "po",
         month(full_date) == 5
  ) %>% 
  ggplot(aes(x = hour, y = cars_per_hour)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~year(full_date), ncol = 2)
