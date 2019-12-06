# Clip the friction surface to AOIs for DECA
# Author: Tim Essam, GeoCenter
# Date: 2019_12_04
# Notes:


# Dependencies ----------------------------------------------------- -------
# Notes: This script requires objects created by the COL_ICT_maps script. Please run that script first
source(file.path(rpath, "COL_ICT_maps.R"))
devtools::install_github("clauswilke/ggtext")
pacman::p_load(raster, sf, rnaturalearth, rnaturalearthhires, rnaturalearthdata, ggspatial, ggtext)



# Load spatial data and crop to Colombia Admin 0 --------------------------
# Notes - the friction surface basically shows where mountains are. The travel time surface is probably better to use

# Load the global friction surface
#friction <- raster(file.path(gispath, "2015_friction_surface_v1.geotiff"))
travel <- raster(file.path(gispath, "2015_accessibility_to_cities_v1.0", "2015_accessibility_to_cities_v1.0.tif"))

# Clip the raster to the Colombia AOI
COL_admin0 <- ne_countries(scale = "large", returnclass = "sf") %>% filter(sovereignt == "Colombia")

# Note that the order for the bbox is xmin, xmax, ymin, ymax not same as st_bbox()
st_bbox(COL_admin0)
bbox <- c(-83, -55, -15, 15)
COL_travel <- crop(travel, bbox)
plot(COL_travel)
res(COL_travel)

COL_travel_rs <- raster::aggregate(COL_travel, fact = 2)


# Resample the raster to a coarser resolution - do not need all th --------



# Convert this into a data frame to plot w/ ggplot using geom_tile
COL_travel_df <- as(COL_travel_rs, "SpatialPixelsDataFrame") %>% as.data.frame(.) %>% 
  rename(travel = `X2015_accessibility_to_cities_v1.0`)


# Map elements, titles etc. -----------------------------------------------


map_description <- c("This global accessibility map enumerates land-based travel time to the nearest densely-populated area for all areas between 85 degrees north and 60 degrees south for a nominal year 2015.")

source <- str_c("Created by USAID GeoCenter on ", today(),  "     |     Source: A global map of travel time to cities to assess inequalities in accessibility in 2015: Accessibility to Cities")
title <- c("Colombia Accessibility to Cities 2015")
subtitle <- c("Each pixel in the  accessibility map represents the modeled shortest time from that location to a city.")



# Plot friction surface and touch up map ----------------------------------

# NOTES: If you want to get a feathered inner/outer glow you can use a series
# of admin0s layered with different stroke thickness and color



col_travel <- 
  terrain  + 
  geom_tile(data = COL_travel_df %>% filter(travel > 0), aes(x = x, y = y, fill = travel)) +
  scale_fill_viridis_c(direction = -1, option = "B", na.value = "NA", alpha = 0.75, trans = "log") +
  #geom_sf(data = ne_colombia, colour = "white", fill = "NA", size = 0.25) +

  geom_sf(data = world_chop, fill = "#d9d9d9", alpha = 0.35, size = 0.25, colour = "#969696") +
  geom_sf(data = world_chop, fill = "NA", colour = "#f9f9f9", size = 0.5, alpha = 0.85) +
  geom_sf(data = col_admin0, colour = "white", fill = "NA", size = 0.9) +
  geom_sf(data = col_admin0, colour = "black", fill = "NA") +
  geom_sf_text_repel(data = ne_cities %>% filter(sov0name == "Colombia", str_detect(featurecla, "Admin-0*")), 
                     aes(label = name)) +
  #geom_sf_text_repel(data = ne_colombia, aes(label = Departmento)) +
  map_clean +
  theme(legend.position = "none") +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
  labs(x = "", y = "", 
       title = title,
       subtitle = subtitle,
       caption = source)

ggsave(file.path(imagepath, "COL_travel_surface_2015.pdf"),
       plot = col_travel,
       height = 23.4,
       width = 16.5,
       unit = "in",
       useDingbats = F,
       dpi = "retina")
  

