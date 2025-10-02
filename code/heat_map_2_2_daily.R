# Load necessary libraries
library(dplyr)      # For data manipulation
library(ggthemes)   # For additional ggplot2 themes
library(ggplot2)    # For data visualization
library(viridis)    # For color scales
library(scico)      # For custom scientific color scales
library(forcats)    # For handling categorical data

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define file path for oblasts data
oblasts_file_path <- file.path(
  "data",
  "oblasts.csv"
)

# Read data from CSV file into a data frame
adjusted_pop_sizes_for_oblasts <- read.csv(
  oblasts_file_path,
  header = TRUE
)

# Group and summarize data for oblasts to calculate total population changes
adjusted_pop_sizes_for_oblasts_1 <- adjusted_pop_sizes_for_oblasts %>% 
  group_by(NAME_1, month, day) %>% 
  summarise(median_daily_change = median(net_pop_change, na.rm = TRUE), .groups = "drop") %>% 
  arrange(NAME_1, month, day)

# Create a new column NAME_1_v2 with updated names
adjusted_pop_sizes_for_oblasts_1$NAME_1_v2 <- gsub("'", "", adjusted_pop_sizes_for_oblasts_1$NAME_1) 

# Define replacements
replacements <- c(
  "Khmelnytskyy" = "Khmelnytskyi",
  "Mykolayiv" = "Mykolaiv",
  "Vinnytsya" = "Vinnytsia",
  "Zaporizhia" = "Zaporizhzhia"
)

# Apply replacements
adjusted_pop_sizes_for_oblasts_1$NAME_1_v2 <- ifelse(
  adjusted_pop_sizes_for_oblasts_1$NAME_1_v2 %in% names(replacements),
  replacements[adjusted_pop_sizes_for_oblasts_1$NAME_1_v2],
  adjusted_pop_sizes_for_oblasts_1$NAME_1_v2
)

# Identify key winners based on total net migration
key_winners <- adjusted_pop_sizes_for_oblasts_1 %>% 
  group_by(NAME_1_v2) %>% 
  reframe(tot_net_migration = sum(median_daily_change)) %>% 
  arrange(tot_net_migration)

# Define order of oblasts based on key winners
key_winners_order <- unique(key_winners$NAME_1_v2)

# Convert oblasts to a factor with the desired order
adjusted_pop_sizes_for_oblasts_1$oblasts <- factor(adjusted_pop_sizes_for_oblasts_1$NAME_1_v2, levels = key_winners_order)


# Convert numeric month to month names
adjusted_pop_sizes_for_oblasts_1$month <- factor(
  adjusted_pop_sizes_for_oblasts_1$month,
  labels = c(
    "February", "March", "April", "May", "June",
    "July", "August"
  )
)

# Create heatmap using ggplot2
ggplot(adjusted_pop_sizes_for_oblasts_1, 
       aes(x = fct_inorder(paste(month, day, sep = " ")), y = oblasts, fill = median_daily_change)) +
  # Use geom_tile to create the heatmap
  geom_tile(color = "white", linewidth = 0.1) +
  labs(x = "Day", y = "Oblast") +
  # Customize color scale using viridis
  scale_fill_scico("Median Net Migration", palette = "vik", midpoint = 0, labels = scales::comma, guide = guide_colorbar(title.position="top", title.hjust = 0.5)) +
  # Customize the x-axis breaks
  scale_x_discrete(breaks = c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")) +
  # Customize the theme
  theme_minimal() +
  theme_tufte() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size=10, face = "plain", color="black"),
    legend.title.align = -0.5,
    axis.text.y = element_text(size=8, margin = margin(r = 0)),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=10),
    axis.title=element_text(size=12, face="plain")) +
  theme(legend.key.width = unit(2.5, "cm"), 
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size = 12))
