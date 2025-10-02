# Load necessary libraries
library(tidyverse)     # For data manipulation
library(ggthemes)  # For ggplot themes
library(forcats)   # For working with factors
library(gridExtra)
library(showtext)
library(scico)  # For color scale

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load fonts
font_add_google("Bitter","bit")
font_add_google("Roboto Condensed", "robotocondensed")
showtext_auto()

# Specify file paths for data
returns_file_path <- file.path("data", "returns.csv")

# Read data from CSV files
returns_df <- read.csv(returns_file_path, header = TRUE) %>% 
  filter(from_NAME_1 != "", from_NAME_1 != "?") %>% 
  filter(origin_month != 9, origin_month != 2)

plot_percentage_for_oblast <- function(oblast_name) {
  # Filter data for the specific oblast
  oblast_data <- subset(returns_df, from_NAME_1 == oblast_name)
  
  # Calculate total count for each day and month
  total_counts <- aggregate(count ~ origin_month + origin_day, returns_df, sum)
  names(total_counts)[3] <- "total_count"
  
  # Merge the oblast-specific data with total counts
  merged_data <- merge(
    oblast_data,
    total_counts,
    by = c("origin_month", "origin_day")
  )
  
  # calculate percentage
  merged_data$percentage <- (merged_data$count / merged_data$total_count) * 100
  
  # Convert month numbers to month names
  merged_data$origin_month <- factor(
    merged_data$origin_month,
    labels = c("March", "April", "May", "June", "July", "August")
  )
  
  # Create a date string for x-axis labels
  merged_data$date <- with(merged_data, paste(origin_month, origin_day, sep = " "))
  
  # Create the bar plot
  ggplot(merged_data, aes(x = fct_inorder(date), y = percentage)) +
    geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
    labs(title = paste("Percentage of counts for", oblast_name),
         x = "Date",
         y = "Percentage of Total Count") +
    theme_tufte() +
    scale_x_discrete(breaks = c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")) +
    scale_y_continuous(labels = function(x) ifelse(x == 0, "0", x)) +
    theme(axis.text.x = element_text(size = 10), 
          axis.text.y = element_text(size = 12))
  
}

plot_percentage_for_oblast("Kiev City")

################## Create Weekly Heatmap ############################

# Define file paths for oblasts and raions data
oblasts_file_path <- file.path(
  "data",
  "oblasts.csv"
)

# Read data from CSV files into data frames
month_day_weeks <- read_csv(
  oblasts_file_path) %>% select(month, day, week, week_adjusted)

returns_df <- left_join(
  returns_df,
  month_day_weeks,
  by = c("origin_month" = "month", "origin_day" = "day")
)


# Step 1: Aggregate the data to get total counts for each oblast, month, and day
aggregated_df <- returns_df %>%
  group_by(from_NAME_1, week_adjusted) %>%
  summarize(total_count_oblast = sum(count), .groups = 'drop')

# Step 2: Calculate the total count for each month and day
total_counts <- aggregated_df %>%
  group_by(week_adjusted) %>%
  summarize(total_count = sum(total_count_oblast), .groups = 'drop')

# Step 3: Calculate the percentage for each oblast
returns_percentage <- aggregated_df %>%
  left_join(total_counts, by = c("week_adjusted")) %>%
  mutate(percentage = (total_count_oblast / total_count) )

# Define weeks of interest and corresponding date ranges
weeks_of_interest_2 <- c(2, 6, 10, 15, 19, 24)
date_ranges_2 <- c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")

# Create heatmap using ggplot2
ggplot(returns_percentage, aes(x = week_adjusted, y = from_NAME_1, fill = percentage)) +
  geom_tile(color = "white", linewidth = 0.1) +
  labs(x = "Week", y = "Oblast") +
  scale_fill_scico("Percentage \n of Returns", 
                   palette = "vik", 
                   midpoint = 0, 
                   labels = scales::percent, 
                   guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  scale_x_continuous(
    breaks = weeks_of_interest_2,
    labels = date_ranges_2,
    expand = c(0, 0)
  ) +
  theme_tufte() +
  theme(
    text = element_text(family = "robotocondensed"),
    legend.position = "right",
    legend.title = element_text(size = 25, face = "plain", color = "black"),
    legend.title.align = -0.5,
    axis.text.y = element_text(size = 15),  # Adjust text size for y-axis
    axis.text.x = element_text(size = 15),  # Adjust text size for x-axis
    axis.title = element_text(size = 20, face = "bold")
  ) +
  theme(
    legend.key.width = unit(1, "cm"), 
    legend.key.height = unit(2.5, "cm"),
    legend.text = element_text(size = 12)  # Adjust legend text size
  )

###########################
#####

# returns_df_3 %>% 
#   filter(date > as.Date('2022-03-06')) %>% 
#   ggplot(aes(x = date, y = percent)) +
#   #geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
#   stat_smooth(span = 0.1, se = TRUE, size=1.5, colour = "#E69F00") +
#   stat_smooth(data = filter(returns_df_3_kiev, date > as.Date('2022-03-06')), 
#                             span = 0.1, se = TRUE, size=1.5, colour = "#56B4E9") +
#   stat_smooth(data = filter(returns_df_3_kharkiv, date > as.Date('2022-03-06')), 
#               span = 0.1, se = TRUE, size=1.5, colour = "#009E73") + 
#   annotate("text", x = as.Date('2022-08-25'), y = 0.07,
#            label = "bold('Kiev')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#56B4E9", size = 12) +
#   annotate("text", x = as.Date('2022-08-25'), y = 0.004,
#            label = "bold('National')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#E69F00", size = 12) +
#   annotate("text", x = as.Date('2022-08-25'), y = 0.022,
#            label = "bold('Kharkiv')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#009E73", size = 12) +
#   labs(x = "Date", y = "Rate of Returns \n (percent)") +
#   theme_tufte() +
#   theme(text = element_text(family = "robotocondensed"),
#         axis.text.x = element_text(size = 35), 
#         axis.text.y = element_text(size = 35),
#         axis.title = element_text(size = 44, lineheight = .3)) +
#   scale_x_date(date_labels = "%b %d", breaks = "1 month")  +
#   scale_y_continuous(labels = function(x) ifelse(x == 0, "0", x)) 
# 
# ggsave("../manuscript/figures/2_3/fig3.png", width = 10, height = 6)
# ggsave("../manuscript/figures/2_3/fig3_white.png", width = 10, height = 6, bg = "white")