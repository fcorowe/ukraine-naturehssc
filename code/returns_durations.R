# Load necessary libraries
library(tidyverse)
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
joined_before_after <- readRDS("data/joined_before_after.rds")  # Load the data from an RDS file

geom_histogram(color="black", fill="white")

p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_violin(trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.2))

# Create the first density plot using ggplot2
joined_before_after %>% 
  ggplot(aes(x = duration_weeks)) +
  geom_histogram(aes(y=..density..), position="identity", color = "#440154FF", fill = "#440154FF", size = .8, alpha=0.5) +
  geom_density(alpha=0.5, fill = "#440154FF") +
  geom_vline(xintercept = 8, colour = "grey75", 
             linetype="dashed", size = 1) +
  annotate("text", x = 16, y = 0.08,
           label = "bold('Average number \n of weeks')", parse = TRUE, vjust = -1, hjust = 0.5, color = "black", size = 6.5) +
  annotate( 'curve', x = 16, y = 0.095, xend = 9, yend = 0.11, linewidth = 1, curvature = 0.5, arrow = arrow(length = unit(0.5, 'cm')) ) +
  labs(x = "Number of weeks", y = "Density", colour = "Legend") +  # Axis labels and title 
  theme_tufte() +
  theme(
    text = element_text(family = "robotocondensed"),  # Set font for all text elements
    legend.position = "right",  # Position the legend on the right
    legend.title = element_text(size = 28, face = "plain", color = "black"),  # Customize legend title
    legend.title.align = -0.5,  # Align legend title
    legend.key.width = unit(1, "cm"),  # Set legend key width
    legend.key.height = unit(2.5, "cm"),  # Set legend key height
    legend.text = element_text(size = 20),  # Customize legend text size
    axis.text.y = element_text(size = 22),  # Customize y-axis text size
    axis.text.x = element_text(size = 22),  # Customize x-axis text size
    axis.title = element_text(size = 30, face = "bold")  # Customize axis title
  )


png(filename = "../outputs/sm/returns-duration.png", units="in", width=14, height=10, res=300, bg = "transparent")
  last_plot()
dev.off()
