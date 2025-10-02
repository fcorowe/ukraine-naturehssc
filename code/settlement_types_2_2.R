# Load necessary libraries
library(dplyr)     # For data manipulation
library(ggplot2)   # For data visualization
library(ggthemes)   # For additional ggplot2 themes

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Specify file path for settlement types data
settlement_types_file_path <- file.path("data", "settlement_types.csv")

# Read settlement types data from CSV
settlement_types_df <- read.csv(settlement_types_file_path, header = TRUE)

# Define the order of months
month_order <-
  c(
    "January",
    "February - Before War",
    "February - After War",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August"
  )

# Group settlement types data by month, calculate percentage, and re-order months
settlement_types_df_1 <- settlement_types_df %>%
  group_by(month_2) %>%
  mutate(
    percentage = (total_num_of_people / sum(total_num_of_people)) * 100
  ) %>%
  ungroup() %>%
  mutate(
    month_2 = factor(month_2, levels = month_order)
  )

# Define colors for different settlement types
settlement_colors <- RColorBrewer::brewer.pal(9, "Set1")

# Create a bar plot for percentage of settlement types over months
ggplot(settlement_types_df_1, aes(x = month_2, y = percentage, fill = settlement_type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "",
    x = "Month",
    y = "Percentage",
    fill = "Settlement Type"
  ) +
  scale_fill_manual(values = settlement_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  theme_tufte()
