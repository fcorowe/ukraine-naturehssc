# Load necessary libraries
library(tidyverse)     # For data manipulation
library(ggthemes)  # For ggplot themes
library(forcats)   # For working with factors
library(gridExtra)
library(showtext)

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load fonts
font_add_google("Bitter","bit")
font_add_google("Roboto Condensed", "robotocondensed")
showtext_auto()

# Specify file paths for data
returns_file_path <- file.path("data", "returns.csv")
home_locations_file_path <- file.path("data", "count_of_home_locs_before_war.csv")

# Read data from CSV files
returns_df <- read.csv(returns_file_path, header = TRUE)
home_locations_df <- read.csv(home_locations_file_path, header = TRUE)

# Calculate total number of users
total_users <- home_locations_df %>%
  filter(loc_NAME_1 != "", loc_NAME_1 != "?") %>%
  reframe(total_users = sum(count))

# Merge returns data with home locations data
returns_df_1 <- returns_df %>%
  mutate(loc_NAME_1 = tolower(from_NAME_1),
         loc_NAME_2 = tolower(from_NAME_2)) %>%
  left_join(home_locations_df, by = c("loc_NAME_1", "loc_NAME_2")) %>%
  filter(from_NAME_1 != "", from_NAME_1 != "?") %>%
  mutate(percent = count.x * 100 / count.y)

# Group returns data by month and day
returns_df_2 <- returns_df_1 %>%
  group_by(origin_month, origin_day) %>%
  filter(origin_month != 9) %>%
  reframe(total_returns = sum(count.x)) %>%
  arrange(origin_month, origin_day)

# Add total users to returns data
returns_df_2$total_users <- total_users$total_users

# Calculate percentage of returns
returns_df_3 <- returns_df_2 %>%
  mutate(percent = total_returns * 100 / total_users)

# Define order of months
returns_df_3$origin_month <- factor(
  returns_df_3$origin_month,
  labels = c("February", "March", "April", "May", "June", "July", "August")
)

# add a date column
returns_df_3 <- returns_df_3 %>%
  mutate(
    date = as.Date(paste("2022", origin_month, origin_day, sep = "-"), format = "%Y-%B-%d")
  )

# Create a bar plot of percentage of returns over time
returns_df_3 %>% 
  filter(date > as.Date('2022-03-06')) %>% 
  ggplot(aes(x = date, y = percent)) +
  #geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
  stat_smooth(span = 0.1, se = TRUE, size=1.5, colour = "#1a80bb") +
  labs(x = "Date", y = "Percent of Returns") +
  theme_tufte() +
  theme(text = element_text(family = "robotocondensed"),
        axis.text.x = element_text(size = 35), 
        axis.text.y = element_text(size = 35),
        axis.title = element_text(size = 38, lineheight = .3)) +
  scale_x_date(date_labels = "%b %d", breaks = "1 month")  +
  scale_y_continuous(labels = function(x) ifelse(x == 0, "0", x)) 
  
ggsave("../manuscript/figures/2_3/all_oblasts_returns.jpg", width = 10, height = 6)

# Function to plot returns by oblast with optional annotations
plot_returns_by_oblast <- function(active_oblast, first_date = "", first_date_description = "", second_date = "", second_date_description = "") {
  # Filter home locations data for the active oblast
  total_users_active_oblast <- home_locations_df %>%
    filter(loc_NAME_1 != "", loc_NAME_1 != "?") %>%
    filter(loc_NAME_1 == tolower(active_oblast)) %>%
    reframe(total_users = sum(count))
  
  # Merge returns data with home locations data for the active oblast
  returns_df_1_active_oblast <- returns_df %>%
    mutate(loc_NAME_1 = tolower(from_NAME_1),
           loc_NAME_2 = tolower(from_NAME_2)) %>%
    filter(loc_NAME_1 == tolower(active_oblast)) %>%
    left_join(home_locations_df, by = c("loc_NAME_1", "loc_NAME_2")) %>%
    filter(from_NAME_1 != "", from_NAME_1 != "?") %>%
    mutate(percent = count.x * 100 / count.y)
  
  # Group returns data by month and day for the active oblast
  returns_df_2_active_oblast <- returns_df_1_active_oblast %>%
    group_by(origin_month, origin_day) %>%
    filter(origin_month != 9) %>%
    reframe(total_returns = sum(count.x)) %>%
    arrange(origin_month, origin_day)
  
  # Add total users to returns data for the active oblast
  returns_df_2_active_oblast$total_users <- total_users_active_oblast$total_users
  
  # Calculate percentage of returns for the active oblast
  returns_df_3_active_oblast <- returns_df_2_active_oblast %>%
    mutate(percent = total_returns * 100 / total_users) %>% 
    filter(origin_month != 2)
  
  # Define order of months for the active oblast
  returns_df_3_active_oblast$origin_month <- factor(
    returns_df_3_active_oblast$origin_month,
    labels = c("March", "April", "May", "June", "July", "August")
  )
  
  # Create a bar plot of percentage of returns over time for the active oblast
  ggplot(returns_df_3_active_oblast, aes(x = fct_inorder(paste(origin_month, origin_day, sep = " ")), y = percent)) +
    geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
    labs(x = "Date", y = "Percentage of Devices Detected at Home Location (%)") +
    theme_tufte() +
    scale_x_discrete(breaks = c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")) +
    scale_y_continuous(labels = function(x) ifelse(x == 0, "0", x)) +
    theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 12)) #+
    # Add vertical dotted lines at specific x-values starting from y = 0
    # geom_segment(
    #   aes(x = first_date, y = 0, xend = first_date, yend = max(percent)),
    #   linetype = "dotted",
    #   color = "black"
    # ) +
    # # Annotate the lines with labels
    # annotate(
    #   "text",
    #   x = first_date,
    #   y = max(returns_df_3_active_oblast$percent),
    #   label = first_date_description,
    #   vjust = -1,
    #   hjust = 0.5,
    #   color = "black",
    #   size = 3
    # ) +
    # # Add vertical dotted lines at specific x-values starting from y = 0
    # geom_segment(
    #   aes(x = second_date, y = 0, xend = second_date, yend = max(percent)),
    #   linetype = "dotted",
    #   color = "black"
    # ) +
    # # Annotate the lines with labels
    # annotate(
    #   "text",
    #   x = second_date,
    #   y = max(returns_df_3_active_oblast$percent),
    #   label = second_date_description,
    #   vjust = -1,
    #   hjust = 0.5,
    #   color = "black",
    #   size = 3
    # )
}

# Example usage of the plot_returns_by_oblast function with annotations
plot_returns_by_oblast(
  "kiev city",
  "March 4",
  "Russian Bombing",
  "April 2",
  "Kiev Region Recaptured"
)

ggsave("../manuscript/figures/2_3/kiev_city_returns.jpg", width = 10, height = 6)

# Another example usage of the plot_returns_by_oblast function with annotations
plot_returns_by_oblast(
  "kharkiv", 
  "March 22", 
  "Cluster Bombing",
  "May 11",
  "Major Ukrainian Counteroffensive"
)

ggsave("../manuscript/figures/2_3/kharkiv_returns.jpg", width = 10, height = 4)

###########################
############## Kiev

total_users_active_oblast <- home_locations_df %>%
  filter(loc_NAME_1 != "", loc_NAME_1 != "?") %>%
  filter(loc_NAME_1 == tolower("kiev city")) %>%
  reframe(total_users = sum(count))

# Merge returns data with home locations data for the active oblast
returns_df_1_active_oblast <- returns_df %>%
  mutate(loc_NAME_1 = tolower(from_NAME_1),
         loc_NAME_2 = tolower(from_NAME_2)) %>%
  filter(loc_NAME_1 == tolower("kiev city")) %>%
  left_join(home_locations_df, by = c("loc_NAME_1", "loc_NAME_2")) %>%
  filter(from_NAME_1 != "", from_NAME_1 != "?") %>%
  mutate(percent = count.x * 100 / count.y)

# Group returns data by month and day for the active oblast
returns_df_2_active_oblast <- returns_df_1_active_oblast %>%
  group_by(origin_month, origin_day) %>%
  filter(origin_month != 9) %>%
  reframe(total_returns = sum(count.x)) %>%
  arrange(origin_month, origin_day)

# Add total users to returns data for the active oblast
returns_df_2_active_oblast$total_users <- total_users_active_oblast$total_users

# Calculate percentage of returns for the active oblast
returns_df_3_kiev <- returns_df_2_active_oblast %>%
  mutate(percent = total_returns * 100 / total_users) %>% 
  filter(origin_month != 2)

# Define order of months
returns_df_3_kiev$origin_month <- factor(returns_df_3_kiev$origin_month,
  labels = c("March", "April", "May", "June", "July", "August"))

# add a date column
returns_df_3_kiev <- returns_df_3_kiev %>%
  mutate(
    date = as.Date(paste("2022", origin_month, origin_day, sep = "-"), format = "%Y-%B-%d")
  )


###########################
############## Kharkiv

total_users_active_oblast <- home_locations_df %>%
  filter(loc_NAME_1 != "", loc_NAME_1 != "?") %>%
  filter(loc_NAME_1 == tolower("kharkiv")) %>%
  reframe(total_users = sum(count))

# Merge returns data with home locations data for the active oblast
returns_df_1_active_oblast <- returns_df %>%
  mutate(loc_NAME_1 = tolower(from_NAME_1),
         loc_NAME_2 = tolower(from_NAME_2)) %>%
  filter(loc_NAME_1 == tolower("kharkiv")) %>%
  left_join(home_locations_df, by = c("loc_NAME_1", "loc_NAME_2")) %>%
  filter(from_NAME_1 != "", from_NAME_1 != "?") %>%
  mutate(percent = count.x * 100 / count.y)

# Group returns data by month and day for the active oblast
returns_df_2_active_oblast <- returns_df_1_active_oblast %>%
  group_by(origin_month, origin_day) %>%
  filter(origin_month != 9) %>%
  reframe(total_returns = sum(count.x)) %>%
  arrange(origin_month, origin_day)

# Add total users to returns data for the active oblast
returns_df_2_active_oblast$total_users <- total_users_active_oblast$total_users

# Calculate percentage of returns for the active oblast
returns_df_3_kharkiv <- returns_df_2_active_oblast %>%
  mutate(percent = total_returns * 100 / total_users) %>% 
  filter(origin_month != 2)

# Define order of months
returns_df_3_kharkiv $origin_month <- factor(returns_df_3_kharkiv $origin_month,
                                         labels = c("March", "April", "May", "June", "July", "August"))

# add a date column
returns_df_3_kharkiv  <- returns_df_3_kharkiv  %>%
  mutate(
    date = as.Date(paste("2022", origin_month, origin_day, sep = "-"), format = "%Y-%B-%d")
  )

###########################
#####

returns_df_3 %>% 
  filter(date > as.Date('2022-03-06')) %>% 
  ggplot(aes(x = date, y = percent)) +
  #geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
  stat_smooth(span = 0.1, se = TRUE, size=1.5, colour = "#E69F00") +
  stat_smooth(data = filter(returns_df_3_kiev, date > as.Date('2022-03-06')), 
                            span = 0.1, se = TRUE, size=1.5, colour = "#56B4E9") +
  stat_smooth(data = filter(returns_df_3_kharkiv, date > as.Date('2022-03-06')), 
              span = 0.1, se = TRUE, size=1.5, colour = "#009E73") + 
  annotate("text", x = as.Date('2022-08-25'), y = 0.07,
           label = "bold('Kiev')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#56B4E9", size = 12) +
  annotate("text", x = as.Date('2022-08-25'), y = 0.004,
           label = "bold('National')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#E69F00", size = 12) +
  annotate("text", x = as.Date('2022-08-25'), y = 0.022,
           label = "bold('Kharkiv')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#009E73", size = 12) +
  labs(x = "Date", y = "Rate of Returns \n (percent)") +
  theme_tufte() +
  theme(text = element_text(family = "robotocondensed"),
        axis.text.x = element_text(size = 35), 
        axis.text.y = element_text(size = 35),
        axis.title = element_text(size = 44, lineheight = .3)) +
  scale_x_date(date_labels = "%b %d", breaks = "1 month")  +
  scale_y_continuous(labels = function(x) ifelse(x == 0, "0", x)) 

ggsave("../manuscript/figures/2_3/fig3.png", width = 10, height = 6)
ggsave("../manuscript/figures/2_3/fig3_white.png", width = 10, height = 6, bg = "white")