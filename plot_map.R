#plot map of Kigigak Island
  #black and white satellite image with unnumbered plots on it

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(RODBC)
library(ggplot2)
library(plotly)
library(sf)
library(tidyterra)
library(maptiles)


#read in from the Google earth file of plots
  #this file is the "ykd_NestMonitoringPlots.kmz", change the lcoation where stored
unzip("C:/Users/shoepfner/Desktop/Kigigak 2026/hard drive copy/field_2026/googleEarth_files/ykd_NestMonitoringPlots.kmz", exdir = "kmz_extracted")
polygons <- st_read("kmz_extracted/doc.kml")
st_layers("kmz_extracted/doc.kml") #48 layers
plot(st_geometry(polygons)) #quick visual check

# # Ensure same CRS before joining
# polygons_utm <- st_transform(polygons, crs = 32603)
# polygons_utm_buffered <- st_buffer(polygons_utm, dist = 5)  #5m buffer
# polygons_buffered <- st_buffer(polygons_utm, dist = 5)
# polygons <- polygons_utm_buffered

polygons <- st_transform(polygons, crs = st_crs(points_sf))
#add 5m buffer to prevent no man's land between plots even though they share a border
polygons <- st_buffer(polygons, dist = 5)


#bring in satellite imagery
polygons_3857 <- st_transform(polygons, 3857)
bbox <- st_bbox(polygons_3857)

polygons_buffered <- st_buffer(polygons_3857, dist = 2000)  # adjust distance as needed

sat_tiles <- get_tiles(polygons_buffered, provider = "Esri.WorldImagery", zoom = 14, crop = TRUE)
sat_tiles <- sat_tiles[[1]] * 0.299 + sat_tiles[[2]] * 0.587 + sat_tiles[[3]] * 0.114
names(sat_tiles) <- "gray"
polygons <- polygons_buffered

dev.off()

all <- ggplot() +
  geom_spatraster(data = sat_tiles) +
  scale_fill_gradient(low = "black", high = "white", guide = "none") +
  geom_sf(data = polygons_3857, fill = NA, color = "white", linewidth = 0.3) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
    axis.title = element_blank()) +
  theme(axis.text.x = element_blank(), # remove x-axis labels
    axis.text.y = element_blank())
all

#use plotly to zoom in, not needed for this
ggplotly(all)

#save as an image
ggsave("plot_map.png", plot = all, width = 7, height = 5, units = "in", dpi = 300)
