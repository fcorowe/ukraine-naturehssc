# Load necessary libraries
library(dplyr)      # For data manipulation
library(ggthemes)   # For additional ggplot2 themes
library(ggplot2)    # For data visualization
library(forcats)    # For handling categorical data

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define file path for raions data
raions_file_path <- file.path("data", "raions.csv")

# Read data from CSV file into a data frame
adjusted_pop_sizes_for_raions <- read.csv(raions_file_path, header = TRUE)

# Group and summarize data for raions to calculate total population changes
adjusted_pop_sizes_for_raions_1 <- adjusted_pop_sizes_for_raions %>%
  group_by(NAME_1, NAME_2, month) %>%
  summarise(median_monthly_change = median(net_pop_change, na.rm = TRUE), .groups = "drop") %>%
  arrange(NAME_1, NAME_2, month)

adjusted_pop_sizes_for_raions_1 <- adjusted_pop_sizes_for_raions_1[complete.cases(adjusted_pop_sizes_for_raions_1),]

# Create a new column NAME_1_v2 with updated names
adjusted_pop_sizes_for_raions_1$NAME_1_v2 <- gsub("'", "", adjusted_pop_sizes_for_raions_1$NAME_1) 

# Define replacements
replacements <- c(
  "Khmelnytskyy" = "Khmelnytskyi",
  "Mykolayiv" = "Mykolaiv",
  "Vinnytsya" = "Vinnytsia",
  "Zaporizhia" = "Zaporizhzhia"
)

# Apply replacements
adjusted_pop_sizes_for_raions_1$NAME_1_v2 <- ifelse(
  adjusted_pop_sizes_for_raions_1$NAME_1_v2 %in% names(replacements),
  replacements[adjusted_pop_sizes_for_raions_1$NAME_1_v2],
  adjusted_pop_sizes_for_raions_1$NAME_1_v2
)

# Define a function for linear interpolation
lin_interp = function(x, y, length.out = 20000) {
  approx(x, y, xout = seq(min(x), max(x), length.out = length.out))$y
}

# Interpolate data for smoother plots
interpolated_data <- adjusted_pop_sizes_for_raions_1 %>%
  group_by(NAME_1_v2, NAME_2) %>%
  reframe(
    month_interpolated = lin_interp(month, month),
    median_monthly_change_interpolated = lin_interp(month, median_monthly_change)
  )

# Create a grouping variable for each positive/negative segment
cat.rle = rle(interpolated_data$median_monthly_change_interpolated < 0)
interpolated_data$group = rep.int(1:length(cat.rle$lengths), times = cat.rle$lengths)

# Add a column to indicate net outcome (positive/negative)
interpolated_data <- interpolated_data %>% mutate(net_outcome = if_else(group %% 2 == 1, "neg", "pos"))

# Identify key winners based on total net migration
key_winners <- adjusted_pop_sizes_for_raions_1 %>%
  group_by(NAME_1_v2, NAME_2) %>%
  reframe(tot_net_migration = sum(median_monthly_change)) %>%
  mutate(combo = paste(NAME_1_v2, NAME_2, sep = " - ")) %>%
  arrange(-tot_net_migration) %>%
  slice(1:10)

# Identify key losers based on total net migration
key_losers <- adjusted_pop_sizes_for_raions_1 %>%
  group_by(NAME_1_v2, NAME_2) %>%
  reframe(tot_net_migration = sum(median_monthly_change)) %>%
  mutate(combo = paste(NAME_1_v2, NAME_2, sep = " - ")) %>%
  arrange(tot_net_migration) %>%
  slice(1:10)

# Filter interpolated data for key winners and key losers
interpolated_data_winners <- interpolated_data %>%
  filter(NAME_1_v2 %in% key_winners$NAME_1_v2, NAME_2 %in% key_winners$NAME_2) %>%
  mutate(combo = paste(NAME_1_v2, NAME_2, sep = " - "))

interpolated_data_losers <- interpolated_data %>%
  filter(NAME_1_v2 %in% key_losers$NAME_1_v2, NAME_2 %in% key_losers$NAME_2) %>%
  mutate(combo = paste(NAME_1_v2, NAME_2, sep = " - "))

# Define a function to plot line plots
plot_line_plots <- function(interpolated_data_2, winners_losers) {
  
  result <- ggplot(
    interpolated_data_2,
    aes(x = month_interpolated, 
        y = median_monthly_change_interpolated,
        group = group
    )
  ) +
    geom_line(aes(color = factor(combo)), linewidth = 0) +
    geom_ribbon(
      aes(
        ymin = pmin(median_monthly_change_interpolated, 0),
        ymax = pmax(median_monthly_change_interpolated, 0),
        fill = net_outcome
      ),
      alpha = 1,
      position = "identity",
      show.legend = FALSE
    ) +
    labs(x = "Month", y = "Median Net Migration") +
    scale_y_continuous(
      labels = function(x)
        ifelse(x == 0, "0", scales::label_number(scale = 1e-6, suffix = "M")(x))
    ) +
    scale_x_continuous(breaks = seq(3, max(interpolated_data$month_interpolated), by = 1)) +
    scale_fill_manual(values = c("lightblue", "lightcoral"),
                      name = "fill") +
    theme_minimal() +
    theme_tufte() +
    facet_wrap(
      ~factor(combo, levels = c(winners_losers$combo)),
      scales = "fixed",
      nrow = 2
    ) +  # Set the order of levels
    theme(strip.text = element_text(face = "bold", size = 9),
          legend.position = "none")
  return(result)
}

# Plot line plots for winners and losers
plot_line_plots(interpolated_data_winners, key_winners)
plot_line_plots(interpolated_data_losers, key_losers)
