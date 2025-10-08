# Load necessary libraries
library(dplyr)         # For data manipulation
library(ggthemes)      # For additional ggplot2 themes
library(ggplot2)       # For data visualization
library(sf)            # For handling spatial data
library(classInt)      # For classification of data
library(RColorBrewer)  # For color palettes
library(patchwork)     # For arranging plots
library(showtext)

# Set font style
  # load font
font_add_google("Roboto Condensed", "robotocondensed")
  # automatically use showtext to render text
showtext_auto()

# Theme for maps
theme_map <- function(...) {
  theme_tufte() +
    theme(
      text = element_text(family = "robotocondensed"),
      # remove all axes
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
}


#Theme for plots
theme_tufte2 <- function(...) {
  theme_tufte() +
    theme(
      text = element_text(family = "robotocondensed"),
    )
}

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Specify file paths for shapefiles
file_path_shp_raions <- file.path("UKR_cod_shp", "ukr_admbnda_adm2_sspe_20240416.shp")
file_path_shp_oblasts <- file.path("UKR_cod_shp", "ukr_admbnda_adm1_sspe_20240416.shp")

# Read shapefile data as 'sf' objects
ukr_shp_obj_raions <- st_read(file_path_shp_raions)
ukr_shp_obj_oblasts <- st_read(file_path_shp_oblasts)


# Read raions data from CSV
raions_file_path <- file.path("data", "updated_raions.csv")
adjusted_pop_sizes_for_raions <- read.csv(raions_file_path, header = TRUE)

# Group and summarize raions data to calculate median net population change
adjusted_pop_sizes_for_raions_1 <- adjusted_pop_sizes_for_raions %>%
  group_by(ADM1_EN, ADM2_EN, month) %>%
  reframe(median_net_pop_change = median(net_pop_change))

# # Create a copy of the summarized data
# adjusted_pop_sizes_for_raions_2 <- adjusted_pop_sizes_for_raions_1
# 
# # Order median_net_pop_change as an ordered factor
# adjusted_pop_sizes_for_raions_2$median_net_pop_change <- factor(
#   adjusted_pop_sizes_for_raions_2$median_net_pop_change,
#   levels = unique(adjusted_pop_sizes_for_raions_2$median_net_pop_change)[c(10, 1:9, 11)],
#   ordered = TRUE
# )

# Merge raions shapefile data with summarized raions data
ukr_shp_obj_raions_2 <- merge(
  ukr_shp_obj_raions,
  adjusted_pop_sizes_for_raions_1 %>% filter(!is.na(month)),
  by = c("ADM1_EN", "ADM2_EN")
)

# Create a copy of the merged data
ukr_shp_obj_raions_3 <- ukr_shp_obj_raions_2

# Define the number of classes for classification
num_classes <- 5

# Define the midpoint for classification
midpoint <- 0

# Split the data into segments above and below the midpoint
above_midpoint <- ukr_shp_obj_raions_3 %>% filter(median_net_pop_change > midpoint)
below_midpoint <- ukr_shp_obj_raions_3 %>% filter(median_net_pop_change <= midpoint)

# Perform Fisher-Jenks natural breaks classification for segments above the midpoint
jenks_classes_above <- classIntervals(above_midpoint$median_net_pop_change, n = num_classes, style = "fisher")
breaks_above <- round(jenks_classes_above$brks)

# Perform Fisher-Jenks natural breaks classification for segments below the midpoint
jenks_classes_below <- classIntervals(below_midpoint$median_net_pop_change, n = num_classes, style = "fisher")
breaks_below <- round(jenks_classes_below$brks)

# Assign classification labels to data points
above_midpoint$jenks_classes <- cut(above_midpoint$median_net_pop_change, breaks = breaks_above, include.lowest = TRUE, labels = FALSE)
below_midpoint$jenks_classes <- cut(below_midpoint$median_net_pop_change, breaks = breaks_below, include.lowest = TRUE, labels = FALSE)

# Add an offset to the labels for segments below the midpoint
below_midpoint$jenks_classes <- below_midpoint$jenks_classes + max(above_midpoint$jenks_classes, na.rm = TRUE)

# Combine the segmented data
ukr_shp_obj_raions_4 <- rbind(above_midpoint, below_midpoint)

# Convert the classification labels to ordered factors
ukr_shp_obj_raions_4$jenks_classes <- factor(ukr_shp_obj_raions_4$jenks_classes, ordered = TRUE)

# Create a column to store the ranges of the classifications
class_ranges <- sapply(1:(length(breaks_above) - 1), function(i) {
  paste(format(breaks_above[i], big.mark = ","), "to", format(breaks_above[i + 1], big.mark = ","))
})
class_ranges <- c(class_ranges, sapply(1:(length(breaks_below) - 1), function(i) {
  paste(format(breaks_below[i], big.mark = ","), "to", format(breaks_below[i + 1], big.mark = ","))
}))
ukr_shp_obj_raions_4$class_ranges <- class_ranges[ukr_shp_obj_raions_4$jenks_classes]

# Function to extract the lower bound of each range
get_lower_bound <- function(range_str) {
  lower_bound_str <- strsplit(range_str, " to ")[[1]][1]
  # Remove commas from the string and convert to numeric
  as.numeric(gsub(",", "", lower_bound_str))
}

# Extract lower bounds
lower_bounds <- sapply(class_ranges, get_lower_bound)

# Sort class ranges based on lower bounds
sorted_class_ranges <- class_ranges[order(lower_bounds)]

# Convert class ranges to an ordered factor
ukr_shp_obj_raions_4$class_ranges <- factor(ukr_shp_obj_raions_4$class_ranges, ordered = TRUE, levels = unique(sorted_class_ranges))

# Function to plot the final map
plot_final_map <- function(active_month) {
  filtered_sf_object <- ukr_shp_obj_raions_4 %>% filter(month == active_month)  %>% filter(!is.na(class_ranges))
  
  num_of_color_hues <- length(unique(filtered_sf_object$class_ranges))
  
  # Create a diverging color palette
  color_palette <- rev(brewer.pal(10, "RdBu"))
  
  # Plot the map
  net_migration_plot <- filtered_sf_object %>%
    ggplot() +
    geom_sf(aes(fill = class_ranges), color = "NA", show.legend = FALSE) +
    scale_fill_manual(name = "Net migration", values = color_palette, guide = "legend", drop = FALSE, na.value = "grey90") +
    theme_map() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank()) +
    coord_sf(datum = NA) +
    geom_sf(data = ukr_shp_obj_oblasts, 
            color = "white", 
            fill = "grey70", 
            alpha = 0.2, 
            linewidth = 1) # +
    # geom_sf_text(data = ukr_shp_obj_oblasts, 
    #              aes(label = ADM1_EN), 
    #              colour = "grey35", 
    #              check_overlap = F, 
    #              size = 4)
  
  
  bar_legend <- filtered_sf_object %>%
    ggplot(aes(x = class_ranges)) +
    geom_bar(aes(fill = class_ranges), show.legend = FALSE) +
    scale_fill_manual(values = color_palette, 
                      guide = "none", 
                      drop = FALSE) +
    scale_x_discrete(name = "Net migration") +
    scale_y_continuous(name = "# of areas") +
    theme_tufte2() +
    theme(axis.title = element_text(size = 12, face ="plain"),
          axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_flip()
  
  net_migration_plot_final <- net_migration_plot # + inset_element(bar_legend, l = 0, b = 0.1, r = .3, t = .4)
  
  return(net_migration_plot_final)
}

# Plot the final map for a specified month
plot_final_map(5)


png(filename = "../manuscript/figures/2_2/map_february.png", units="in", width=14, height=10, res=300, bg = "transparent")
  plot_final_map(2)
dev.off()

png(filename = "../manuscript/figures/2_2/map_may.png", units="in", width=14, height=10, res=300, bg = "transparent")
  plot_final_map(5)
dev.off()

png(filename = "../manuscript/figures/2_2/map_august.png", units="in", width=14, height=10, res=300, bg = "transparent")
  plot_final_map(8)
dev.off()


legend_plot <- function(active_month) {
  filtered_sf_object <- ukr_shp_obj_raions_4 %>% filter(month == active_month)  %>% filter(!is.na(class_ranges))
  
  num_of_color_hues <- length(unique(filtered_sf_object$class_ranges))
  
  # Create a diverging color palette
  color_palette <- rev(brewer.pal(10, "RdBu"))
  
  bar_legend <- filtered_sf_object %>%
    ggplot(aes(x = class_ranges)) +
    geom_bar(aes(fill = class_ranges), show.legend = FALSE) +
    scale_fill_manual(values = color_palette, 
                      guide = "none", 
                      drop = FALSE) +
    scale_x_discrete(name = "Net migration") +
    scale_y_continuous(name = "# of areas") +
    theme_tufte2() +
    theme(axis.title = element_text(size = 80, face = "plain"),
          axis.text = element_text(size = 70),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_flip()
  
  net_migration_plot_final <- bar_legend
  
  return(net_migration_plot_final)
}


png(filename = "../manuscript/figures/2_2/legend-map_february.png", units="in", width=18, height=10, res=300, bg = "transparent")
  legend_plot(2)
dev.off()


#### No labels
legend_plot_nolabel <- function(active_month) {
  filtered_sf_object <- ukr_shp_obj_raions_4 %>% filter(month == active_month)  %>% filter(!is.na(class_ranges))
  
  num_of_color_hues <- length(unique(filtered_sf_object$class_ranges))
  
  # Create a diverging color palette
  color_palette <- rev(brewer.pal(10, "RdBu"))
  
  bar_legend <- filtered_sf_object %>%
    ggplot(aes(x = class_ranges)) +
    geom_bar(aes(fill = class_ranges), show.legend = FALSE) +
    scale_fill_manual(values = color_palette, 
                      guide = "none", 
                      drop = FALSE) +
    scale_x_discrete(name = "Net migration") +
    scale_y_continuous(name = "# of areas") +
    theme_tufte2() +
    theme(axis.title = element_text(size = 80, face ="plain"),
          axis.text = element_text(size = 70),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          #axis.ticks.y = element_blank()
          ) +
    coord_flip()
  
  net_migration_plot_final <- bar_legend
  
  return(net_migration_plot_final)
}

png(filename = "../manuscript/figures/2_2/legend-map_may.png", units="in", width=14, height=10, res=300, bg = "transparent")
  legend_plot_nolabel(5)
dev.off()

png(filename = "../manuscript/figures/2_2/legend-map_august.png", units="in", width=14, height=10, res=300, bg = "transparent")
  legend_plot_nolabel(8)
dev.off()
