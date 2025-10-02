# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(ggthemes)   # For additional ggplot2 themes

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the CSV file into a dataframe
combined_data_3 <- read_csv("data/settlements_updated.csv")

# Set the levels for the settlement_type factor in a specific order
combined_data_3$settlement_type <- factor(combined_data_3$settlement_type, levels = c(
  "Dense Urban Cluster", "Low Density Rural", "Rural Cluster", "Semi-Dense Urban Cluster",
  "Suburban", "Unknown", "Urban Centre", "Very Low Density Rural", "Water"
))

combined_data_3 <- combined_data_3 %>% 
  group_by(month, day) %>% 
  mutate(daily_percent = total_num_of_people * 100 / sum(total_num_of_people)) %>% 
  arrange(month, day)

# Ensure 'week' is treated as a factor for discrete x-axis representation
combined_data_3$week <- as.factor(combined_data_3$week)

# Create the plot
p <- ggplot(combined_data_3, aes(x = week, y = daily_percent, color = settlement_type, group = settlement_type)) +
  # Add boxplots, without outliers, slightly transparent, and dodged horizontally
  geom_boxplot(aes(group = interaction(week, settlement_type)), outlier.shape = NA, alpha = 0.2, position = position_dodge(width = 0.8)) +
  # Add jittered points for individual observations, slightly transparent
  geom_jitter(width = 0.1, alpha = 0.4) +
  # Add lines for the mean values for each settlement type, dodged horizontally
  stat_summary(fun = mean, geom = "line", aes(group = settlement_type), size = 1, position = position_dodge(width = 0.8)) +
  # Add points for the mean values for each settlement type, dodged horizontally
  stat_summary(fun = mean, geom = "point", aes(group = settlement_type), size = 2, position = position_dodge(width = 0.8)) +
  # Set axis labels and title
  labs(x = "Week", y = "Daily percent", title = "") +
  # Apply Tufte theme for a clean, minimalist look
  theme_tufte() +
  # Rotate x-axis text for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 35),
        axis.title = element_text(size = 40),
        text = element_text(size = 30),
        legend.position = "none")

# Define custom colors for each settlement type
color_values <- c(
  "Dense Urban Cluster" = "#AA4499", "Low Density Rural" = "#117733", "Rural Cluster" = "#332288", 
  "Semi-Dense Urban Cluster" = "#44AA99", "Suburban" = "#882255", "Unknown" = "gray",
  "Urban Centre" = "#CC6677", "Very Low Density Rural" = "#DDCC77", "Water" = "#88CCEE"
)

# Apply the custom colors to the plot
p <- p + scale_color_manual(values = color_values)

# Annotate the plot with text and a vertical line
p + 
  annotate("text", x = 8, size = 15, y = 70, label = "Start of invasion", color = "grey50", angle = 90, vjust = 1.5) +  # Add annotation text
  geom_vline(xintercept = 8, linetype = "dashed", color = "grey50") +  # Add a vertical dashed line
  # Adjust y-axis scale and limits, and format y-axis labels to show percentage
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 100)) 


png(filename = "../manuscript/figures/2_2/settlement-areas_boxplot.png", units="in", width=24, height=10, res=300, bg = "transparent")
  last_plot()
dev.off()
