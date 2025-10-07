# Load necessary libraries
library(tidyverse)
library(sf)
library(ggthemes)     # For additional ggplot2 themes
library(showtext)     # For using Google fonts in plots
library(viridisLite)
library(scales)
library(kableExtra)

# Load Google fonts
font_add_google("Bitter", "bit")                   # Add 'Bitter' font and assign it alias 'bit'
font_add_google("Roboto Condensed", "robotocondensed")  # Add 'Roboto Condensed' font and assign it alias 'robotocondensed'
showtext_auto()  # Automatically use 'showtext' for text rendering

# Set the working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in the data
returns_flow_data_3 <- readRDS("data/returns_flow_data_3.rds")  # Load the data from an RDS file


# Create the first density plot using ggplot2
returns_flow_data_3 %>% 
  filter(distance_km > 0 & distance_km < 500) %>% 
  ggplot(aes(x = distance_km)) +
  # All Oblasts
  geom_density(aes(group = home_oblast, colour = "Oblasts"), colour = "grey80", alpha = 0.5, show.legend = FALSE) +
  # Kiev
  geom_density(data = returns_flow_data_3 %>% filter(home_oblast == "Kiev City"), colour = "#E69F00", size = 1, show.legend = FALSE) +
  stat_density(data = returns_flow_data_3 %>% filter(home_oblast == "Kiev City"), colour = "#E69F00", geom = "line", position = "identity", size = 1) +
  # National
  geom_density(aes(colour = "National"), colour = "#440154FF", size = 1.5, show.legend = FALSE) +
  stat_density(aes(colour = "National"), colour = "#440154FF", geom = "line", position = "identity", size = 1.5) +
  # Annotation as a legend
  annotate("text", x = 50, y = 0.03,
           label = "bold('All oblasts')", parse = TRUE, vjust = -1, hjust = 0.5, color = "grey80", size = 10) +
    annotate("text", x = 100, y = 0.01,
           label = "bold('National')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#440154FF", size = 10) +
  annotate("text", x = 500, y = 0.0016,
           label = "bold('Kiev')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#E69F00", size = 10) +
  labs(x = "Distance (km)", y = "Density", colour = "Legend") +  # Axis labels and title
  theme_tufte() +  # Use Tufte theme
  theme(
    text = element_text(family = "robotocondensed"),  # Set font for all text elements
    legend.position = "right",  # Position the legend on the right
    legend.title = element_text(size = 28, face = "plain", color = "black"),  # Customize legend title
    legend.title.align = -0.5,  # Align legend title
    legend.key.width = unit(1, "cm"),  # Set legend key width
    legend.key.height = unit(2.5, "cm"),  # Set legend key height
    legend.text = element_text(size = 20),  # Customize legend text size
    axis.text.y = element_text(size = 25),  # Customize y-axis text size
    axis.text.x = element_text(size = 25),  # Customize x-axis text size
    axis.title = element_text(size = 34, face = "bold")  # Customize axis title
  )

png(filename = "../manuscript/figures/2_3/distance_all-moves.png", units="in", width=14, height=10, res=300, bg = "transparent")
  last_plot()
dev.off()

#####################################################
## RETURNS

# Filter data for those who returned home after war
returns_flow_data_3_returned <- returns_flow_data_3 %>% 
  filter(returned_home_after_war == TRUE)

returns_flow_data_3_returned %>% 
  filter(distance_km > 0 & distance_km < 500) %>% 
  ggplot(aes(x = distance_km)) +
  # All Oblasts
  geom_density(aes(group = home_oblast, colour = "Oblasts"), colour = "grey80", alpha = 0.5, show.legend = FALSE) +
  # Kiev
  geom_density(data = returns_flow_data_3_returned %>% filter(home_oblast == "Kiev City"), colour = "#E69F00", size = 1, show.legend = FALSE) +
  stat_density(data = returns_flow_data_3_returned %>% filter(home_oblast == "Kiev City"), colour = "#E69F00", geom = "line", position = "identity", size = 1) +
  # National
  geom_density(aes(colour = "National"), colour = "#440154FF", size = 1.5, show.legend = FALSE) +
  stat_density(aes(colour = "National"), colour = "#440154FF", geom = "line", position = "identity", size = 1.5) +
  # Annotation as a legend
  annotate("text", x = 50, y = 0.03,
           label = "bold('All oblasts')", parse = TRUE, vjust = -1, hjust = 0.5, color = "grey80", size = 10) +
  annotate("text", x = 100, y = 0.01,
           label = "bold('National')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#440154FF", size = 10) +
  annotate("text", x = 500, y = 0.0016,
           label = "bold('Kiev')", parse = TRUE, vjust = -1, hjust = 0.5, color = "#E69F00", size = 10) +
  labs(x = "Distance (km)", y = "Density", colour = "Legend") +  # Axis labels and title
  theme_tufte() +  # Use Tufte theme
  theme(
    text = element_text(family = "robotocondensed"),  # Set font for all text elements
    legend.position = "right",  # Position the legend on the right
    legend.title = element_text(size = 28, face = "plain", color = "black"),  # Customize legend title
    legend.title.align = -0.5,  # Align legend title
    legend.key.width = unit(1, "cm"),  # Set legend key width
    legend.key.height = unit(2.5, "cm"),  # Set legend key height
    legend.text = element_text(size = 20),  # Customize legend text size
    axis.text.y = element_text(size = 25),  # Customize y-axis text size
    axis.text.x = element_text(size = 25),  # Customize x-axis text size
    axis.title = element_text(size = 34, face = "bold")  # Customize axis title
  )

png(filename = "../manuscript/figures/2_3/distance_return-moves.png", units="in", width=14, height=10, res=300, bg = "transparent")
last_plot()
dev.off()

distance_summ <- returns_flow_data_3_returned %>% 
  group_by(home_oblast) %>% 
  drop_na(distance_km) %>% 
  filter(distance_km > 0) %>% 
  summarise(median_distance_km = round( median(distance_km, na.rm = TRUE), 1 ),
            min_distance_km = round( min(distance_km, na.rm = TRUE), 1 ),
    max_distance_km =round( max(distance_km, na.rm = TRUE), 1),
    sd_distance_km =round( sd(distance_km, na.rm = TRUE), 1)) %>% 
  na.omit() 

distance_summ <- distance_summ[order(-distance_summ$median_distance_km),]

distance_summ %>% 
  kbl(booktabs = TRUE, col.names = c("Home oblast", "Median", "Min", "Max", "SD")) %>% 
  kable_paper(full_width = FALSE) %>% 
  kable_styling( font_size = 60) %>% 
  column_spec(1, color = "white", bold = T,
              background = spec_color(distance_summ$median_distance_km, end = 0.7),
              popover = paste("am:", distance_summ$median_distance_km)) %>% 
    column_spec(2, color = "white", bold = T,
              background = spec_color(distance_summ$median_distance_km, end = 0.7),
              popover = paste("am:", distance_summ$median_distance_km)) %>%
  column_spec(3, color = "white", bold = T,
              background = spec_color(distance_summ$median_distance_km, end = 0.7),
              popover = paste("am:", distance_summ$median_distance_km)) %>%
  column_spec(4, color = "white", bold = T,
              background = spec_color(distance_summ$median_distance_km, end = 0.7),
              popover = paste("am:", distance_summ$median_distance_km)) %>%
  column_spec(5, color = "white", bold = T,
              background = spec_color(distance_summ$median_distance_km, end = 0.7),
              popover = paste("am:", distance_summ$median_distance_km)) %>%
  save_kable("../manuscript/figures/2_3/ioms_returns/median-dist_tbl.png")

