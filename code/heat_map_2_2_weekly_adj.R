# Load necessary libraries
library(dplyr)      # For data manipulation
library(ggthemes)   # For additional ggplot2 themes
library(ggplot2)    # For data visualization
library(viridis)    # For color scales
library(scico)      # For custom scientific color scales
library(scales)     # For custom transformation of breaks and labels
library(showtext)

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#setwd("./code/")
# Define file path for oblasts data
oblasts_file_path <- file.path(
  "data",
  "oblasts.csv"
)

# Load fonts
font_add_google("Bitter","bit")
font_add_google("Roboto Condensed", "robotocondensed")
showtext_auto()


# Read data from CSV file into a data frame
adjusted_pop_sizes_for_oblasts <- read.csv(
  oblasts_file_path,
  header = TRUE
)

# Group and summarize data for oblasts to calculate total population changes
adjusted_pop_sizes_for_oblasts_1 <- adjusted_pop_sizes_for_oblasts %>% 
  group_by(NAME_1, week_adjusted) %>% 
  summarise(median_weekly_change = median(net_pop_change, na.rm = TRUE), .groups = "drop") %>% 
  arrange(NAME_1, week_adjusted)

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

# Define the regions for each oblast
# regions <- c(
#   "Cherkasy" = "Central", "Chernihiv" = "Northern", "Chernivtsi" = "Western", "Crimea" = "Southern",
#   "Dnipropetrovsk" = "Eastern", "Donetsk" = "Eastern", "Ivano-Frankivsk" = "Western", "Kharkiv" = "Eastern",
#   "Kherson" = "Southern", "Khmelnytskyi" = "Western", "Kiev" = "Northcentral", "Kiev City" = "Northcentral",
#   "Kirovohrad" = "Central", "Lviv" = "Western", "Luhansk" = "Eastern", "Mykolaiv" = "Southcentral",
#   "Odessa" = "Southcentral", "Poltava" = "Central", "Rivne" = "Northwest", "Sumy" = "Northeast",
#   "Ternopil" = "Western", "Vinnytsia" = "Central", "Volyn" = "Western", "Zakarpattia" = "Western",
#   "Zaporizhzhia" = "Southeast", "Zhytomyr" = "Northern"
# )

regions <- c(
  "Cherkasy", "Chernihiv", "Chernivtsi", "Crimea",
  "Dnipropetrovsk", "Donetsk", "Ivano-Frankivsk", "Kharkiv",
  "Kherson", "Khmelnytskyi", "Kiev", "Kiev City",
  "Kirovohrad", "Lviv", "Luhansk", "Mykolaiv",
  "Odessa", "Poltava", "Rivne", "Sumy",
  "Ternopil", "Vinnytsia", "Volyn", "Zakarpattia",
  "Zaporizhzhia", "Zhytomyr"
)

# Assume your data frame is named 'adjusted_pop_sizes_for_oblasts_1'
adjusted_pop_sizes_for_oblasts_1 <- adjusted_pop_sizes_for_oblasts_1 %>%
  mutate(NAME_1_v2_updated = paste(NAME_1_v2, " (", regions[NAME_1_v2], ")", sep = ""))

# Identify key winners based on total net migration
key_winners <- adjusted_pop_sizes_for_oblasts_1 %>% 
  group_by(NAME_1_v2) %>% 
  reframe(tot_net_migration = sum(median_weekly_change)) %>% 
  arrange(tot_net_migration)

# Define order of oblasts based on key winners
key_winners_order <- unique(key_winners$NAME_1_v2)

# Convert oblasts to a factor with the desired order
adjusted_pop_sizes_for_oblasts_1$oblasts <- factor(adjusted_pop_sizes_for_oblasts_1$NAME_1_v2, levels = key_winners_order)

# Create a new column 'month_name' with month names
adjusted_pop_sizes_for_oblasts_x <- adjusted_pop_sizes_for_oblasts %>%
  select(month, day, week_adjusted) %>% 
  unique() %>% 
  mutate(month_name = month.abb[month]) %>%
  arrange(month, day)

first_dates <- adjusted_pop_sizes_for_oblasts_x %>%
  group_by(week_adjusted) %>%
  summarise_all(first)

last_dates <- adjusted_pop_sizes_for_oblasts_x %>%
  group_by(week_adjusted) %>%
  summarise_all(last)

combined_dates <- left_join(
  first_dates,
  last_dates,
  by = "week_adjusted"
)

# Function to generate date range
generate_week_starting <- function(month_name_first, day_first) {
  week_starting <- paste(month_name_first, day_first)
  return(week_starting)
}

# Apply the function to create the date_range column
combined_dates <- combined_dates %>%
  mutate(week_starting = mapply(generate_week_starting, month_name.x, day.x))

# Extract the date_range values for the specified weeks
weeks_of_interest <- c(1, 5, 10, 15, 20, 25)  # Update with the weeks you are interested in
date_ranges <- combined_dates$`week_starting`[combined_dates$week_adjusted %in% weeks_of_interest]

# Used this delineation to match previous figures
weeks_of_interest_2 <- c(2,6,10,15,19,24)
date_ranges_2 <- c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")

# Create heatmap using ggplot2
ggplot(adjusted_pop_sizes_for_oblasts_1, 
       aes(x = week_adjusted, y = oblasts, fill = median_weekly_change)) +
  # Use geom_tile to create the heatmap
  geom_tile(color = "white", linewidth = 0.1) +
  labs(x = "Week", y = "Oblast") +
  # Customize color scale using viridis
  scale_fill_scico("Net migration \n (median)", 
                   palette = "vik", 
                   midpoint = 0, 
                   #labels = scales::comma, 
                   labels = label_number(scale_cut = cut_short_scale()),
                   guide = guide_colorbar(title.position="top", 
                                          title.hjust = 0.5)) +
  # Customize the x-axis breaks
  scale_x_continuous(
    breaks = weeks_of_interest_2,
    labels = date_ranges_2,
    expand = c(0, 0)
  ) +
  # Customize the theme
  theme_tufte() +
  theme(
    text = element_text(family = "robotocondensed"),
    legend.position = "right",
    legend.title = element_text(size = 25, face = "plain", color="black"),
    legend.title.align = -0.5,
    axis.text.y = element_text(size = 25, margin = margin(r = 0)),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 25),
    axis.title = element_text(size = 35, face ="bold")) +
  theme(legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(2.5, "cm"),
        legend.text = element_text(size = 20))

png("../manuscript/figures/2_2/heat_map_weekly.png", units="in", width=25, height=10, res=300, bg = "transparent")
  last_plot()
dev.off()
