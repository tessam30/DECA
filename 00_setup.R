# Purpose: Set up repo for all things DECA
# Author: Tim Essam, Ph.D. | USAID GeoCenter
# Date: 2018_08_08
# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES - if first time, install pacman
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl",
               "ggmap", "maps", "sf", "rgeos", "llamar", "ggrepel", "purrr")

# Create folders for project (if they do not exist)
folder_list <- list("Data", "Images", "Scripts", "Dataout", "GIS", "Documents", "Graphics", "Tableau")
purrr::map(folder_list, ~dir.create(.))

datapath <- "Data"
dataout <- "Dataout"
gispath <- "GIS"
graphpath <- "Graphics"
imagepath <- "Images"  
rpath <- "Scripts"
tableau <- "Tableau"

# Fix time zone issues
Sys.setenv(TZ = "America/New_York")