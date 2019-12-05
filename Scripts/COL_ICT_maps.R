# Prep Colombia ICT data for maps
# Author: Tim Essam, GeoCenter
# Date: 2019_12_03
# Notes:


pacman::p_load(raster, sp, rgdal, rmapshaper, geojsonio, rnaturalearth, rnaturalearthdata, ggsflabel, tidytext)
#devtools::install_github("ropensci/rnaturalearthhires") # to grab polygons
#devtools::install_github("yutannihilation/ggsflabel")

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


# Natural Earth -----
world <- ne_countries(scale = "large", returnclass = "sf")

# Merging with crossalk so we have mergability with ICT data captured in excel file
# The ISO code for Bogota is incorrect, it should be CO_DC instead of CO_CUN
ne_colombia <- ne_states(country = "colombia", returnclass = "sf") %>% 
  mutate(iso_3166_2 = ifelse(iso_3166_2  == "CO-CUN" & name == "Bogota", "CO-DC", iso_3166_2),
    ISO2 = str_replace(iso_3166_2, "-", "_")) %>% 
  left_join(., col_cw, by = c("ISO2")) %>% 
  filter(ISO2 != "CO_X01~")

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
  geom_sf(data = world_chop, fill = "#d9d9d9", alpha = 0.35, size = 0.25, colour = "#969696") +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) 
  #geom_sf_text(data = world_chop, aes(label = sovereignt)) +
  #geom_sf(data = col_admin0, colour = "black", alpha = 0.75) 



# Choropleths on top of basemaps ------------------------------------------
source <- str_c("Maps created by USAID GeoCenter on ", today(), "     |     Data source: National Administrative Department of Statistics of Colombia 2018")

theme_set(theme_minimal())
map_clean <- theme(legend.position = "top",
                            axis.text = element_blank(),
                            panel.grid = element_blank(),
                            axis.ticks = element_blank(),
                            strip.text = element_text(hjust = 0))
  

basemap_color <- "#EEE7D7"

# Basemap of Colombia for reference
base_map <- oth_countries +
  geom_sf(data = ne_colombia, fill = '#EEE7D7', size = 0.25, colour = "#525252", alpha = 0.6) +
  geom_sf_text_repel(data = ne_colombia, aes(label = Dept_geo), colour = "black", size = 3) +
  geom_sf(data = col_admin0, colour = "white", fill = "NA", size = 1) +
  geom_sf(data = col_admin0, colour = "black", fill = "NA", size = 0.5) +
  map_clean + theme(legend.position = "none") +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
  labs(x = "", y = "", caption = str_c("Created by USAID GeoCenter on ", today()))

ggsave(file.path(imagepath, "COL_basemap_admin2.pdf"), plot = base_map,
       height = 11, width = 8.5, dpi = "retina", units = "in", useDingbats = F)

# Helper function for joins ----------------------------------------------

join_ict <- function(df, filter) {
  df_long <- 
    df %>% 
    gather(stat, value, 2:length(df)) %>% 
    left_join(., ne_colombia, by = c("Departmento")) %>% 
    mutate(label_first = ifelse(stat == filter, Departmento, NA_character_),
           value = round((value / 100), 2), # ensure our legends only have 2 digits
           stat = str_replace(stat," \\(%\\)", "")) # strip out the (%) at the end of stats
  return(df_long)
}

# Return a faceted map for each table
map_plot <- function(df, vircolor = "B", title = "") {
  p <- 
    oth_countries + 
    geom_sf(data = df, 
            aes(fill = value, geometry = geometry), size = 0.25, colour = "white") +
    geom_sf(data = col_admin0, colour = "white", fill = "NA", size = 0.9) +
    geom_sf(data = col_admin0, colour = "black", fill = "NA") +
    #geom_sf_label(data = tvs, aes(label = label_first, geometry = geometry), size = 2.5) +
    facet_wrap(~stat) +
    scale_fill_viridis_c(option = vircolor, direction = -1, alpha = 0.80, label = scales::percent_format()) +
    map_clean +
    coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
    labs(x = "", y = "", title = title,
         caption = source,
         fill = "Percent of households") 
  return(p)
}




# Table 1 - TV ownership --------------------------------------------------
# What is in the ICT list?
purrr::map(ict, ~names(.))

tvs <- join_ict(ict$Table1, filter = "Televisión a color convencional (%)")
tv_maps <- map_plot(tvs, vircolor = "C", title = "Table 1. Television owernship by different types")


# Bar graphs  
  tvs %>% 
  mutate(dept_ordered = tidytext::reorder_within(Departmento, value, stat)) %>% 
  ggplot(aes(y = value, x = dept_ordered, fill = value)) + geom_col()  + coord_flip() +
    facet_wrap(~stat, scales = "free_y") +
  scale_fill_viridis_c(option = "A", direction = -1, label = scales::percent_format(1)) +
    scale_x_reordered() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "", y = "",
         fill = "Percent of households") +
    theme(legend.position = "top")
    

ggsave(file.path(imagepath, "COL_tv_ownership.pdf"), plot = tv_maps,
       height = 11, width = 8.5, dpi = "retina", useDingbats = F)




# Table 3 - Mobile phone ownership  ------
phone <- join_ict(ict$`Table 3`, filter = "Teléfono celular")
phone_map <- map_plot(phone, vircolor = "A", title = "Mobile phone ownership")

ggsave(file.path(imagepath, "COL_mobile_ownership.pdf"), plot = phone_map,
       height = 11, width = 8.5, dpi = "retina", useDingbats = F)


# Table 4 - computer ownership
computer <- join_ict(ict$`Table 4`, filter = "Computador portátil")
comp_map <- map_plot(computer, vircolor = "D", title = "Computer ownership by type")

ggsave(file.path(imagepath, "COL_computer_ownership.pdf"), plot = comp_map,
       height = 11, width = 8.5, dpi = "retina", useDingbats = F)

# Table 5 - Internet connection ----
internet <- join_ict(ict$`Table 5`, filter = "Hogares con Internet")
int_map <- map_plot(internet, vircolor = "C", title = "Internet connection by type")

# Table 6 - Radio use
radio <- join_ict(ict$`Table 6`, filter = "Entretenimiento")
radio_map <- map_plot(radio %>% filter(stat != "Otra"), vircolor = "D", title = "Radio use patterns")      

ggsave(file.path(imagepath, "COL_radio_use.pdf"), plot = radio_map,
       height = 11, width = 8.5, dpi = "retina", useDingbats = F)


# Table 7 - Cell phone use reason -----
mobile_use <- join_ict(ict$`Table 7`, filter = "Llamadas personales o familiares")
mob_map <- map_plot(mobile_use, vircolor = "A", title = "Mobile phone use patterns")

# Table 8 - Computer use anywhere -----
comp_use <- join_ict(ict$`Table 8`, filter = "Computador de Escritorio")
comp_use_map <- map_plot(comp_use, vircolor = "D", title = "Computer use anywhere by device type")


