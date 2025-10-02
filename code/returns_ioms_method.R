library(tidyverse)   # For data manipulation and visualization

# Set the working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the returns data
returns_df <- read_csv("data/returns.csv")

# Load and process home locations data
home_locations_df <- read_csv("data/count_of_home_locs_before_war.csv") %>% 
  mutate(loc_NAME_1 = ifelse(loc_NAME_1 == "ivano", "ivano-frankivs'k", loc_NAME_1))

# Load oblasts data
oblasts_df <- read_csv("data/oblasts.csv")

# Create a dataframe with month, day, and weeks information
month_day_weeks <- oblasts_df %>% 
  select(month, day, week, week_adjusted)

# Add week information to returns data
returns_df <- returns_df %>% 
  filter(origin_month != 9) %>% 
  left_join(month_day_weeks, by = c("origin_month" = "month", "origin_day" = "day"))

# Group and summarize the returns data by origin and adjusted week
returns_df_1 <- returns_df %>% 
  filter(!is.na(from_NAME_1), from_NAME_1 != "?") %>% 
  group_by(from_NAME_1, week_adjusted) %>% 
  reframe(weekly_returns = sum(count, na.rm = TRUE))

# Summarize the home locations data
home_locations_df_2 <- home_locations_df %>% 
  filter(!is.na(loc_NAME_1), loc_NAME_1 != "?") %>% 
  group_by(loc_NAME_1) %>% 
  reframe(count_of_home_location = sum(count, na.rm = TRUE))

# Merge returns data with home locations data and calculate proportions
returns_df_2 <- returns_df_1 %>%
  mutate(loc_NAME_1 = tolower(from_NAME_1)) %>%
  left_join(home_locations_df_2, by = c("loc_NAME_1")) %>% 
  mutate(proportion = weekly_returns / count_of_home_location)

# Extract and merge baseline population data
oblasts_baseline_pop <- oblasts_df %>% 
  select(NAME_1, total_baseline_oblast_population) %>% 
  distinct()

# Merge and calculate updated weekly counts and proportions
returns_df_3 <- returns_df_2 %>% 
  left_join(oblasts_baseline_pop, by = c("from_NAME_1" = "NAME_1")) %>% 
  mutate(updated_weekly_count = proportion * total_baseline_oblast_population) %>% 
  mutate(pcnt_returns_over_baseline_oblast_pop = updated_weekly_count * 100 / total_baseline_oblast_population) %>% 
  select(-c(loc_NAME_1))

# Summarize total weekly returns
total_weekly_returns <- returns_df_3 %>% 
  group_by(week_adjusted) %>% 
  reframe(total_weekly_count = sum(updated_weekly_count, na.rm = TRUE))

# Merge total weekly returns and calculate final proportions
returns_df_4 <- returns_df_3 %>% 
  left_join(total_weekly_returns, by = c("week_adjusted")) %>% 
  mutate(pcnt_returns_over_total_ukr_weekly_returns_pop = updated_weekly_count * 100 / total_weekly_count)

# Write to disk
write_csv(
  returns_df_4,
  file.path("data", "returns_ioms_method.csv")
)
