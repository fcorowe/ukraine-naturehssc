# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(stars)      # For handling spatial raster data
library(sf)         # For handling spatial vector data
library(ggthemes)   # For additional ggplot2 themes
library(viridisLite)
library(scales)
library(showtext)

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load fonts
font_add_google("Bitter","bit")
font_add_google("Roboto Condensed", "robotocondensed")
showtext_auto()

# File paths for input data
ukr_tif_file_path <- "ukrainev4.tif"
ukr_boundary_file_path <- "gadm41_UKR_shp/gadm41_UKR_0.shp"
ukr_raions_boundary_file_path <- "UKR_cod_shp/ukr_admbnda_adm2_sspe_20240416.shp"

# Read raster data with specified buffer size for faster processing
ukr_tif <- read_stars(ukr_tif_file_path, RasterIO = list(nBufXSize = 600, nBufYSize = 600))

# Read vector data for the country boundary and raions (administrative regions)
ukr_boundary <- st_read(ukr_boundary_file_path)
ukr_raions_boundary <- st_read(ukr_raions_boundary_file_path)

# Simplify the geometries to reduce complexity while preserving topology
ukr_raions_boundary <- st_simplify(
  ukr_raions_boundary,
  preserveTopology = TRUE,
  dTolerance = 1000
)

ukr_boundary <- st_simplify(
  ukr_boundary,
  preserveTopology = TRUE,
  dTolerance = 1000
)

# Clip the raster using the vector boundary
ukr_tif_clipped <- st_crop(ukr_tif, ukr_boundary)

# Plot the clipped raster and boundary to verify
plot(ukr_tif_clipped)
plot(st_geometry(ukr_boundary), add = TRUE)

# Define a color palette for the unique values
 color_palette <- c("10" = "#88CCEE", "11" = "#DDCC77", "12" = "#117733", "13" = "#332288", 
                    "21" = "#882255", "22" = "#44AA99", "23" = "#AA4499", 
                    "30" = "#CC6677")



# Define the labels for the legend
 legend_labels <- c("30" = "Urban centre", "23" = "Dense urban cluster", 
                    "22" = "Semi-dense urban cluster", "21" = "Suburban or peri-urban", 
                    "13" = "Rural cluster", "12" = "Low density rural", "11" = "Very low density rural", 
                    "10" = "Water")


# Convert the raster values to a factor
ukr_tif_clipped[[1]] <- as.factor(ukr_tif_clipped[[1]])

# Convert the raster values to a factor with levels ordered as required for plotting
ukr_tif[[1]] <- factor(ukr_tif[[1]], levels = c("30", "23", "22", "21", "13", "12", "11", "10"))

# New categories
ukr_tif_clipped[[1]] <- recode(ukr_tif_clipped[[1]], "30" = "1", "23" = "1", 
                               "22" = "2", "21" = "2", 
                               "13" = "3", "12" = "3", "11" = "3", 
                               "10" = "4")

#new_color_palette <- c("10" = "#88CCEE", "13" = "#3B0F70FF", "21" = "#DE4968FF", "30" = "#FDE725FF")
new_color_palette <- c("1" = "#FE9F6DFF", "2" = "#DE4968FF", "3" = "#3B0F70FF",  "4" = "transparent")

new_labels <- c("1" = "Urban",
                "2" = "Suburban or peri-urban", 
                "3" = "Rural",
                "4" = "")


# Plot with categorical scale and custom legend
ggplot() +
  geom_stars(data = ukr_tif_clipped) +  # Add the raster data to the plot
  scale_fill_manual(values = new_color_palette, 
                    labels = new_labels, 
                    na.value = "transparent",  # Set NA values to be transparent
                    na.translate = FALSE,
                    guide = guide_legend(reverse = FALSE, byrow=TRUE)) +  # Customize the legend
  labs(fill = "Degree of Urbanisation") +  # Set the legend title
  theme_tufte() +
  coord_equal(ratio = 1.5) +  # Maintain aspect ratio
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE)) +  # Ensure legend items are arranged by rows
  theme(
    text = element_text(family = "robotocondensed"),
    legend.position = "right",  # Position the legend on the right
    #legend.key.size = unit(1.2, "lines"),  # Adjust the size of the legend keys (boxes)
    #legend.spacing.y = unit(1, "cm"),  # Add space between the legend items
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.text = element_text( size = 28),
    legend.title = element_text(size=34)
  )  +
  geom_sf(data = ukr_raions_boundary, fill = NA, color = "white", linewidth = 0.3)  # Overlay boundary of raions

png(filename = "../manuscript/figures/2_2/settlement-areas_map.png", units="in", width=14, height=10, res=300, bg = "transparent")
  last_plot()
dev.off()