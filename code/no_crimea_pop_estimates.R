library(tidyverse)

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define file path for oblasts data
oblasts_file_path <- file.path(
  "data",
  "oblasts.csv"
)

# Read the oblasts data from the CSV file
oblasts_df <- read_csv(oblasts_file_path)

# Process the oblasts data
oblasts_df_1 <- oblasts_df %>% 
  # Select all columns except the ones listed
  select(-c(
    mean_pop,
    total_current_pop_by_month_and_day,
    scaling_factor,
    adjusted_pop_size,
    net_pop_change,
    proportional_net_change
  )) %>% 
  # Rename column for clarity
  rename(
    total_ukr_baseline_pop = total_baseline_pop
  ) %>% 
  # Filter out rows where NAME_1 is "Crimea"
  filter(NAME_1 != "Crimea")

# Calculate total current population by month and day
total_current_pop_by_month_and_day <- oblasts_df_1 %>% 
  group_by(month, day) %>% 
  reframe(total_current_pop_by_month_and_day = sum(current_pop_size, na.rm = TRUE))

# Join the total current population by month and day with the main dataframe
oblasts_df_2 <- oblasts_df_1 %>% 
  left_join(
    total_current_pop_by_month_and_day,
    by = c("month", "day")
  )

# Calculate scaling factor for population adjustment
oblasts_df_2$scaling_factor <- (oblasts_df_2$total_ukr_baseline_pop - oblasts_df_2$net_outflow) / oblasts_df_2$total_current_pop_by_month_and_day

# Adjust current population size using the scaling factor
oblasts_df_2$adjusted_pop_size <- oblasts_df_2$scaling_factor * oblasts_df_2$current_pop_size

# Calculate net population change
oblasts_df_2$net_pop_change <- oblasts_df_2$adjusted_pop_size - oblasts_df_2$total_baseline_oblast_population

# Calculate proportional net change in population
oblasts_df_2$proportional_net_change <- oblasts_df_2$net_pop_change / oblasts_df_2$total_baseline_oblast_population

# Write the processed data to a CSV file
write_csv(
  oblasts_df_2,
  "data/no_crimea_oblasts_disp.csv"
)
