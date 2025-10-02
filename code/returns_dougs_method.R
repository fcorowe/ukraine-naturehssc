# Load the tidyverse library for data manipulation
library(tidyverse)

# Set the working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the returns data from CSV file
returns_df <- read_csv("data/returns.csv")

# Load and process home locations data, replace "ivano" with "ivano-frankivs'k" in loc_NAME_1
home_locations_df <- read_csv("data/count_of_home_locs_before_war.csv") %>% 
  mutate(loc_NAME_1 = ifelse(loc_NAME_1 == "ivano", "ivano-frankivs'k", loc_NAME_1))

# Load oblasts data from CSV file
oblasts_df <- read_csv("data/oblasts.csv")

# Group returns data by from_NAME_1, origin_month, and origin_day, and sum the counts
# Filter out rows where from_NAME_1 is "?" or NA
returns_oblasts_daily <- returns_df %>% 
  group_by(from_NAME_1, origin_month, origin_day) %>% 
  reframe(count = sum(count)) %>% 
  filter(from_NAME_1 != "?", !is.na(from_NAME_1))

# Select relevant columns from oblasts data for daily penetration rate calculations
oblasts_daily_pen_rate <- oblasts_df %>% 
  select(NAME_1, month, day, total_baseline_oblast_population, baseline_penetration_rate, scaling_factor, week_adjusted)

# Join the daily returns data with the penetration rate data, calculate adjusted counts
# Adjust count by baseline penetration rate and scaling factor
returns_oblasts_daily_2 <- returns_oblasts_daily %>% 
  left_join(
    oblasts_daily_pen_rate,
    by = c("from_NAME_1" = "NAME_1", "origin_month" = "month", "origin_day" = "day")
  ) %>% 
  mutate(adjusted_count = count / baseline_penetration_rate) %>% 
  mutate(adjusted_count_2 = adjusted_count * scaling_factor)

# Group by adjusted week and sum the adjusted counts to get total weekly returns
returns_weekly <- returns_oblasts_daily_2 %>% 
  group_by(week_adjusted) %>% 
  reframe(total_weekly_count = sum(adjusted_count_2, na.rm = TRUE))

# Further group the daily adjusted returns data by from_NAME_1, week_adjusted, and total baseline population
# Sum the weekly returns and calculate the percentage of returns over baseline population and total weekly returns
returns_oblasts_daily_3 <- returns_oblasts_daily_2 %>% 
  group_by(from_NAME_1, week_adjusted, total_baseline_oblast_population) %>% 
  reframe(weekly_returns = sum(adjusted_count_2, na.rm = TRUE)) %>% 
  left_join(
    returns_weekly,
    by = c("week_adjusted")
  ) %>%
  mutate(
    pcnt_returns_over_baseline_oblast_pop = weekly_returns * 100 / total_baseline_oblast_population
  ) %>%
  mutate(
    pcnt_returns_over_total_ukr_weekly_returns_pop = weekly_returns * 100 / total_weekly_count
  )

# Write to disk
write_csv(
  returns_oblasts_daily_3,
  file.path("data", "returns_dougs_method.csv")
)
