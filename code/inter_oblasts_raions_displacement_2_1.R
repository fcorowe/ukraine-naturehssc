# Load necessary libraries
library(dplyr)      # For data manipulation
library(ggthemes)   # For additional ggplot2 themes
library(ggplot2)    # For data visualization
library(forcats)    # For handling categorical data
library(gridExtra)
library(showtext)

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load fonts
font_add_google("Bitter","bit")
font_add_google("Roboto Condensed", "robotocondensed")
showtext_auto()

# Define file paths for oblasts and raions data
oblasts_file_path <- file.path(
  "data",
  "oblasts.csv"
)

raions_file_path <- file.path(
  "data",
  "raions.csv"
)

# Read data from CSV files into data frames
adjusted_pop_sizes_for_oblasts <- read.csv(
  oblasts_file_path,
  header = TRUE
)

adjusted_pop_sizes_for_raions <- read.csv(
  raions_file_path,
  header = TRUE
)

# Filter, arrange, and summarize data for oblasts to calculate total population declines
adjusted_pop_sizes_for_oblasts_1 <- adjusted_pop_sizes_for_oblasts %>%
  filter(net_pop_change < 0) %>%
  arrange(NAME_1, month, day)

adjusted_pop_sizes_for_oblasts_2 <- adjusted_pop_sizes_for_oblasts_1 %>%
  group_by(month, day) %>%
  summarise(total_pop_declines = sum(net_pop_change, na.rm = TRUE), .groups = "drop") %>%
  arrange(month, day) %>%
  collect() # Collect data from Spark DataFrame to R data.frame

# Convert numeric month to month names
adjusted_pop_sizes_for_oblasts_2$month <- factor(
  adjusted_pop_sizes_for_oblasts_2$month,
  labels = c(
    "February", "March", "April", "May", "June",
    "July", "August"
  )
)

# Change the signs of the total_pop_declines values to positive
adjusted_pop_sizes_for_oblasts_2$total_pop_declines <- abs(adjusted_pop_sizes_for_oblasts_2$total_pop_declines)

# add a date column
adjusted_pop_sizes_for_oblasts_2 <- adjusted_pop_sizes_for_oblasts_2 %>%
  mutate(
    date = as.Date(paste("2022", month, day, sep = "-"), format = "%Y-%B-%d")
  )

# Create the bar graph using ggplot2 for oblasts
oblasts_bar <- ggplot(adjusted_pop_sizes_for_oblasts_2, 
                      aes(x = date, y = total_pop_declines)) +
  #geom_line(linewidth = 1.5) +
  # Add vertical dotted lines at specific x-values starting from y = 0
  annotate("text", x = as.Date('2022-03-08'), y = max(adjusted_pop_sizes_for_oblasts_2$total_pop_declines) * 0.9,
           label = "(1) Initial Evacuation", vjust = -1, hjust = 0.2, color = "grey20", size = 9) +
  geom_segment(aes(x = as.Date('2022-03-08'), y = 0, xend = as.Date('2022-03-08'), yend = max(total_pop_declines)  * 0.9),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  # Annotate the lines with labels
  annotate("text", x = as.Date('2022-04-24'), y = max(adjusted_pop_sizes_for_oblasts_2$total_pop_declines) * 0.9,
           label = "(2) Easter Sunday", vjust = -1, hjust = 0.5, color = "grey20", size = 9) +
  geom_segment(aes(x = as.Date('2022-04-24'), y = 0, xend = as.Date('2022-04-24'), yend = max(total_pop_declines) * 0.9),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  # Annotate the lines with labels
  annotate("text", x = as.Date('2022-05-14'), y = max(adjusted_pop_sizes_for_oblasts_2$total_pop_declines) * 0.9,
           label = "(3) Khersonska evacuation #1", vjust = -1, hjust = 0.1, color = "grey20", size = 9) +
  # Add vertical dotted lines at specific x-values starting from y = 0
  geom_segment(aes(x = as.Date('2022-05-14'), y = 0, xend = as.Date('2022-05-14'), yend = max(total_pop_declines) * 0.9),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  # Annotate the lines with labels
  annotate("text", x = as.Date('2022-08-01'), y = max(adjusted_pop_sizes_for_oblasts_2$total_pop_declines) * 0.9,
           label = "(4) Battle of Bakhmut", vjust = -1, hjust = 0.5, color = "grey20", size = 9) +
  geom_segment(aes(x = as.Date('2022-08-01'), y = 0, xend = as.Date('2022-08-01'), yend = max(total_pop_declines) * 0.9),
               linetype = "dashed", color = "grey75", linewidth = 0.1)  +
  stat_smooth(span = 0.08, se = TRUE, size=1.5, colour = "darkblue") +
  #geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
  labs(x = NULL, y = "Estimates of population displacement \n (Number of People)") +
  scale_y_continuous(labels = function(x) ifelse(x == 0, "0", scales::label_number(scale = 1e-6, suffix = "M")(x))) +
  theme_tufte() +
  scale_x_date(date_labels = "%b %d", breaks = "1 month") +
  #scale_x_discrete(breaks = c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")) +
  theme(text = element_text(family = "robotocondensed"),
        axis.text.x = element_text(size = 35), 
        axis.text.y = element_text(size = 35),
        axis.title = element_text(size = 38, lineheight = .3),
        plot.margin = unit(c(10,10,10,10), "pt")
        )


oblasts_bar

ggsave("../manuscript/figures/2_1/oblasts.jpg", width = 8, height = 4)


# Filter, arrange, and summarize data for raions to calculate total population declines
adjusted_pop_sizes_for_raions_1 <- adjusted_pop_sizes_for_raions %>%
  filter(net_pop_change < 0) %>%
  arrange(NAME_1, NAME_2, month, day)

adjusted_pop_sizes_for_raions_2 <- adjusted_pop_sizes_for_raions_1 %>%
  group_by(month, day) %>%
  summarise(total_pop_declines = sum(net_pop_change, na.rm = TRUE), .groups = "drop") %>%
  arrange(month, day) %>%
  collect() # Collect data from Spark DataFrame to R data.frame

# Convert numeric month to month names
adjusted_pop_sizes_for_raions_2$month <- factor(
  adjusted_pop_sizes_for_raions_2$month,
  labels = c(
    "February", "March", "April", "May", "June",
    "July", "August"
  )
)

# Change the signs of the total_pop_declines values to positive
adjusted_pop_sizes_for_raions_2$total_pop_declines <- abs(adjusted_pop_sizes_for_raions_2$total_pop_declines)

# add a date column
adjusted_pop_sizes_for_raions_2 <- adjusted_pop_sizes_for_raions_2 %>%
  mutate(
    date = as.Date(paste("2022", month, day, sep = "-"), format = "%Y-%B-%d")
  )

# Create the bar graph using ggplot2 for raions
raions_bar <- ggplot(adjusted_pop_sizes_for_raions_2, 
                     aes(x = date, y = total_pop_declines)) +
  #geom_line(linewidth = 1.5) + 
  geom_segment(aes(x = as.Date('2022-03-08'), y = 0, xend = as.Date('2022-03-08'), yend = max(total_pop_declines)),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  # Annotate the lines with labels
  geom_segment(aes(x = as.Date('2022-04-24'), y = 0, xend = as.Date('2022-04-24'), yend = max(total_pop_declines)),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  # Add vertical dotted lines at specific x-values starting from y = 0
  geom_segment(aes(x = as.Date('2022-05-14'), y = 0, xend = as.Date('2022-05-14'), yend = max(total_pop_declines)),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  # Annotate the lines with labels
  geom_segment(aes(x = as.Date('2022-08-01'), y = 0, xend = as.Date('2022-08-01'), yend = max(total_pop_declines)),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  stat_smooth(span = 0.08, se = TRUE, size=1.5, colour = "darkred") +
  labs(x = NULL, y = "Population change relative to pre-war population") +
  scale_y_continuous(labels = function(x) ifelse(x == 0, "0", scales::label_number(scale = 1e-6, suffix = "M")(x))) +
  theme_tufte() +
  scale_x_date(date_labels = "%b %d", breaks = "1 month") +
  #scale_x_discrete(breaks = c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")) +
  theme(text = element_text(family = "robotocondensed"),
        axis.text.x = element_text(size = 35), 
        axis.text.y = element_text(size = 35),
        axis.title = element_text(size = 38, lineheight = .3),
        plot.margin = unit(c(10,10,10,10), "pt")
  ) 

raions_bar

ggsave("../manuscript/figures/2_1/raions.jpg", width = 8, height = 4)


oblasts_raions <- grid.arrange(oblasts_bar, raions_bar, ncol = 1)

#ggsave("../manuscript/figures/2_1/oblasts_raions.jpg", oblasts_raions, height = 10)


ggplot(data = adjusted_pop_sizes_for_raions_2) +
  #geom_line(linewidth = 1.5) +
  annotate("text", x = as.Date('2022-03-08'), y = max(adjusted_pop_sizes_for_raions_2$total_pop_declines) * 0.8,
           label = "(1) Initial Evacuation", vjust = -1, hjust = 0.2, color = "grey20", size = 8) +
  geom_segment(aes(x = as.Date('2022-03-08'), y = 0, xend = as.Date('2022-03-08'), yend = max(total_pop_declines)  * 0.8),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  # Annotate the lines with labels
  annotate("text", x = as.Date('2022-03-31'), y = max(adjusted_pop_sizes_for_raions_2$total_pop_declines) * 0.9,
           label = "(2) Withdrawal from Kyiv & northeastern front", vjust = -1, hjust = 0.5, color = "grey20", size = 8) +
  geom_segment(aes(x = as.Date('2022-03-31'), y = 0, xend = as.Date('2022-03-31'), yend = max(total_pop_declines) * 0.9),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  # Annotate the lines with labels
  annotate("text", x = as.Date('2022-05-16'), y = max(adjusted_pop_sizes_for_raions_2$total_pop_declines) * 0.92,
           label = "(3) Capture of Mariupol", vjust = -1, hjust = 0.1, color = "grey20", size = 8) +
  # Add vertical dotted lines at specific x-values starting from y = 0
  geom_segment(aes(x = as.Date('2022-05-16'), y = 0, xend = as.Date('2022-05-16'), yend = max(total_pop_declines) * 0.92),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  # Annotate the lines with labels
  annotate("text", x = as.Date('2022-06-02'), y = max(adjusted_pop_sizes_for_raions_2$total_pop_declines) * 0.98,
           label = "(4) Invasion of Severodonetsk", vjust = -1, hjust = 0.1, color = "grey20", size = 8) +
  # Add vertical dotted lines at specific x-values starting from y = 0
  geom_segment(aes(x = as.Date('2022-06-02'), y = 0, xend = as.Date('2022-06-02'), yend = max(total_pop_declines) * 0.98),
               linetype = "dashed", color = "grey75", linewidth = 0.1) +
  annotate("text", x = as.Date('2022-08-25'), y = 15000000 * 1,
           label = "bold('Raion')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#ea801c", size = 8) +
  annotate("text", x = as.Date('2022-08-25'), y = 8000000 * 1,
           label = "bold('Oblast')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#1a80bb", size = 8) +
  stat_smooth( data = adjusted_pop_sizes_for_oblasts_2, 
               aes(x = date, y = total_pop_declines),
               span = 0.08, se = TRUE, size=1.5, colour = "#1a80bb") +
  stat_smooth( data = adjusted_pop_sizes_for_raions_2, 
               aes(x = date, y = total_pop_declines),
               span = 0.08, se = TRUE, size=1.5, colour = "#ea801c") +
  #geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
  labs(x = NULL, y = "Estimated population displacement \n (number of people)") +
  scale_y_continuous(labels = function(x) ifelse(x == 0, "0", scales::label_number(scale = 1e-6, suffix = "M")(x))) +
  theme_tufte() +
  scale_x_date(date_labels = "%b %d", breaks = "1 month") +
  #scale_x_discrete(breaks = c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")) +
  theme(text = element_text(family = "robotocondensed"),
        axis.text.x = element_text(size = 35), 
        axis.text.y = element_text(size = 35),
        axis.title = element_text(size = 38, lineheight = .3),
        plot.margin = unit(c(10,10,10,10), "pt"),
        legend.position = "right"
  )

ggsave("../manuscript/figures/2_1/fig1.jpg", width = 10, height = 4)