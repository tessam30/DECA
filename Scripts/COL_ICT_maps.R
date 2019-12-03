# Prep Colombia ICT data for maps
# Author: Tim Essam, GeoCenter
# Date: 2019_12_03
# Notes:


pacman::p_load(raster, sp, rgdal, rmapshaper, geojsonio)
library("rnaturalearth")
library("rnaturalearthdata")

# Loading ICT and shapefile to join and map -------------------------------

source(file.path(rpath, "COL_cw.R"))

geo <- st_read(file.path(gispath, "MGN_DPTO_POLITICO2.shp"), stringsAsFactors = FALSE) %>% 
  left_join(., col_cw, by = c("DPTO_CCDGO")) %>% 
  dplyr::select(matches('Dept*|DEPT*|ISO'), everything()) 
 
# Convert to geo_json for simplification, then back to an sf object for ggplotting
geo_json <- geojson_json(geo, geometry = "polygon", group = "group")  
col_geo <- rmapshaper::ms_simplify(geo_json) %>% geojson_sf(.)


# Load the excel sheet in a single batch writing to a list; Makes for easy handling
read_path <- file.path(file.path(datapath, 'COL_DECA_ICT.xlsx'))

ict <- 
  excel_sheets(read_path) %>%
  set_names() %>% 
  purrr::map(., ~read_excel(., path = read_path))

# Pull in natural earth data -- clipped to colombia AOI
world <- ne_countries(scale = "large", returnclass = "sf")

ne_colombia <- ne_states(country = "colombia", returnclass = "sf") %>% 
  mutate(ISO2 = str_replace(iso_3166_2, "-", "_")) %>% 
  left_join(., col_cw, by = c("ISO2"))


ne_geo <- raster::raster(file.path(gispath, "SR_LR", "SR_LR.tif"))
ne_ocean <- st_read(file.path(gispath, "ne_10m_ocean", "ne_10m_ocean.shp"))



# What is the bounding box we are dealing w/?
st_bbox(geo) 
mapRange <- c(range(st_coordinates(geo)[,1]),range(st_coordinates(geo)[,2]))


# Filtering out extra countries to get our map focused on South America
world_chop <- world %>% 
  filter(sovereignt != "Colombia") %>% 
  filter(sovereignt %in% c("Panama", "Venezuela", "Bolivia", "Ecuador", "Brazil", "Peru"))

col_admin0 <- world %>% filter(sovereignt == "Colombia")


# Crop raster using vectov extent to get nice terrain feature
# crop the lidar raster using the vector extent
# Add a bit of padding the bounding box
ne_ocean_chop <- st_crop(ne_ocean, xmin = -83, ymin = -5, xmax = -65, ymax = 15)
ne_geo_chop <- crop(ne_geo, ne_ocean_chop)

# Need a data frame to get ggplot to render the raster data
spdf <- as(ne_geo_chop, "SpatialPixelsDataFrame") %>% as.data.frame(.)



# Base maps ---------------------------------------------------------------

terrain <- ggplot() +
  geom_tile(data = filter(spdf, SR_LR < 210), aes(x = x, y = y, alpha = SR_LR)) +
  scale_alpha(name = "", range = c(0.6, 0), guide = F) +
  theme(legend.position = "none") +
  geom_sf(data = ne_ocean_chop, fill = "aliceblue", colour = "NA") 

oth_countries <- terrain + 
  geom_sf(data = world_chop, fill = "#d9d9d9", alpha = 0.35, size = 0.5, colour = "#969696") +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) 
  #geom_sf_text(data = world_chop, aes(label = sovereignt)) +
  #geom_sf(data = col_admin0, colour = "black", alpha = 0.75) 



# Choropleths on top of basemaps ------------------------------------------
source <- str_c("Author: Maps created by USAID GeoCenter     |     Source: National Administrative Department of Statistics of Colombia 2018")

tvs <- 
  ict$Table1 %>% 
  gather(tv_stat, value, 2:4) %>% 
  left_join(., ne_colombia, by = c("Departmento")) %>% 
  mutate(label_first = ifelse(tv_stat == "Televisión a color convencional (%)", Departmento, NA_character_),
         value = (value / 100)) 

  oth_countries + 
    geom_sf(data = tvs, 
            aes(fill = value, geometry = geometry), size = 0.25, colour = "white") +
    geom_sf(data = col_admin0, colour = "black", fill = "NA") +
    #geom_sf_label(data = tvs, aes(label = label_first, geometry = geometry), size = 2.5) +
    facet_wrap(~tv_stat) +
    scale_fill_viridis_c(option = "A", direction = -1, alpha = 0.6, label = scales::percent_format()) +
    theme_minimal() +
    theme(legend.position = "top",
          axis.text = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_text(hjust = 0)) +
    coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
    labs(x = "", y = "", title = "Table 1. TV Onwership by different types",
         caption = source) 




  


         