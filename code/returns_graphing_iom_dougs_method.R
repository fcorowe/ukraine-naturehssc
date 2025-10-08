# Load necessary libraries
library(tidyverse)   # For data manipulation and visualization
library(scico)       # For color palettes
library(ggthemes)    # For theme_tufte
library(forcats)     # For working with factors
library(showtext)    # For adding Google fonts
library(scales)      # For number formatting

# Load Google fonts
font_add_google("Bitter", "bit")
font_add_google("Roboto Condensed", "robotocondensed")
showtext_auto()

# Function to create the line plot
create_line_plot <- function(file_name) {
  data_path <- paste0("./code/data/", file_name, ".csv")
  returns_df <- read_csv(data_path)
  
  total_weekly_returns <- returns_df %>% 
    select(week_adjusted, total_weekly_count) %>% 
    distinct()
  
  weeks_of_interest_2 <- c(2, 6, 10, 15, 19, 24)
  date_ranges_2 <- c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")
  
  ggplot(total_weekly_returns %>% filter(week_adjusted < 28), aes(x = week_adjusted, y = total_weekly_count)) +
    stat_smooth(span = 0.3, se = TRUE, size=1.5, colour = "#1a80bb") +
    #geom_line(color = "blue", size = 1) +  # Add a line plot
    #geom_point(color = "red", size = 2) +  # Add points to the line plot
    labs(x = "Week", y = "Total Weekly Count") +
    scale_x_continuous(
      breaks = weeks_of_interest_2,
      labels = date_ranges_2,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_si(unit = ""))  # Format y-axis labels with "M" for millions
    ) +
    theme_tufte() +
    theme(
      text = element_text(family = "robotocondensed"),
      legend.position = "right",
      legend.title = element_text(size = 35, face = "plain", color = "black"),
      legend.title.align = -0.5,
      axis.text.y = element_text(size = 25),  # Adjust text size for y-axis
      axis.text.x = element_text(size = 25),  # Adjust text size for x-axis
      axis.title = element_text(size = 30, face = "bold")
    ) +
    theme(
      legend.key.width = unit(1, "cm"), 
      legend.key.height = unit(0.5, "cm"),
      legend.text = element_text(size = 12)  # Adjust legend text size
    )
}

# Function to create the heatmap of returns over baseline population
create_heatmap_baseline <- function(file_name) {
  data_path <- paste0("./data/", file_name, ".csv")
  returns_df <- read_csv(data_path)
  
  weeks_of_interest_2 <- c(2, 6, 10, 15, 19, 24)
  date_ranges_2 <- c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")
  
  cum_returns_df <- returns_df %>% group_by(from_NAME_1) %>% 
    summarise(cum_returns = sum(weekly_returns)) 
  
  returns_df <- returns_df %>%
    left_join(cum_returns_df, by = "from_NAME_1")
  
  #returns_df$cum_displacements <- sum( (returns_df$pcnt_returns_over_baseline_oblast_pop / 100), rm.na = TRUE)
  
  ggplot(returns_df, aes(x = week_adjusted, y = reorder(from_NAME_1, cum_returns), fill = pcnt_returns_over_baseline_oblast_pop / 100)) +
    geom_tile(color = "white", linewidth = 0.1) +
    labs(x = "Week", y = "Oblast") +
    scale_fill_scico("Returns / \n Baseline Population", 
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
      legend.position = "bottom",
      legend.title = element_text(size = 25, face = "plain", color = "black"),
      legend.title.align = -0.5,
      axis.text.y = element_text(size = 22),  # Adjust text size for y-axis
      axis.text.x = element_text(size = 22),  # Adjust text size for x-axis
      axis.title = element_text(size = 30, face = "bold")
    ) +
    theme(
      legend.key.width = unit(4, "cm"), 
      legend.key.height = unit(0.8, "cm"),
      legend.text = element_text(size = 22)  # Adjust legend text size
    )
}

# Function to create the heatmap of returns over total weekly returns
create_heatmap_weekly <- function(file_name) {
  data_path <- paste0("./data/", file_name, ".csv")
  returns_df <- read_csv(data_path)
  
  weeks_of_interest_2 <- c(2, 6, 10, 15, 19, 24)
  date_ranges_2 <- c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")
  
  cum_returns_df <- returns_df %>% group_by(from_NAME_1) %>% 
    summarise(cum_returns = sum(weekly_returns)) 
  
  returns_df <- returns_df %>%
    left_join(cum_returns_df, by = "from_NAME_1")
  
  ggplot(returns_df, aes(x = week_adjusted, y = reorder(from_NAME_1, cum_returns), fill = pcnt_returns_over_total_ukr_weekly_returns_pop / 100)) +
    geom_tile(color = "white", linewidth = 0.1) +
    labs(x = "Week", y = "Oblast") +
    scale_fill_scico("Returns / \n Total Weekly Returns", 
                     palette = "roma", 
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
      legend.position = "bottom",
      legend.title = element_text(size = 25, face = "plain", color = "black"),
      legend.title.align = -0.5,
      axis.text.y = element_text(size = 22),  # Adjust text size for y-axis
      axis.text.x = element_text(size = 22),  # Adjust text size for x-axis
      axis.title = element_text(size = 30, face = "bold")
    ) +
    theme(
      legend.key.width = unit(4, "cm"), 
      legend.key.height = unit(0.8, "cm"),
      legend.text = element_text(size = 22)  # Adjust legend text size
    )
}

# ggplot(data = returns_df) +
#   geom_col( aes( y = reorder(from_NAME_1, cum_returns), x = cum_returns), fill = "grey50") +
#   scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
#   labs(x = "Total returns") +
#   theme_tufte() +
#    theme(
#     text = element_text(family = "robotocondensed"),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank()
#   )

# For line plot
create_line_plot("returns_dougs_method")

png(filename = "./manuscript/figures/2_3/dougs_returns/ukr_wide_returns2.png", units = "in", width = 14, height = 10, res = 300, bg = "transparent")
  last_plot()
dev.off()

# png(filename = "../manuscript/figures/2_3/ioms_returns/ukr_wide_returns.png", units = "in", width = 14, height = 10, res = 300, bg = "transparent")
#   last_plot()
# dev.off()


# For heatmap of returns over baseline population
create_heatmap_baseline("returns_dougs_method")

png(filename = "../manuscript/figures/2_3/dougs_returns/returns_over_baseline_pop.png", units = "in", width = 16, height = 10, res = 300, bg = "transparent")
last_plot()
dev.off()

# png(filename = "../manuscript/figures/2_3/ioms_returns/returns_over_baseline_pop.png", units = "in", width = 16, height = 10, res = 300, bg = "transparent")
# last_plot()
# dev.off()

# For heatmap of returns over total weekly returns
create_heatmap_weekly("returns_dougs_method")

png(filename = "../manuscript/figures/2_3/dougs_returns/returns_over_total_ukr_wide_returns_pop.png", units = "in", width = 16, height = 10, res = 300, bg = "transparent")
last_plot()
dev.off()

# png(filename = "./manuscript/figures/2_3/ioms_returns/returns_over_total_ukr_wide_returns_pop.png", units = "in", width = 16, height = 10, res = 300, bg = "transparent")
# last_plot()
# dev.off()
