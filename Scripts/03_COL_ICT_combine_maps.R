# Combine pdf plots into a single pdf and reorder
# Author: Tim Essam, GeoCenter
# Date: 2019_12_04
# Notes:


# PDF tools load ----------------------------------------------------------

library(pdftools)

dir(imagepath, pattern = ".pdf")

# What order do you want the maps organized in the pdf?
map_order <- c("COL_basemap_admin2.pdf",
               "COL_mobile_ownership.pdf",
               "COL_mobile_use.pdf",
               "COL_computer_ownership.pdf",
               "COL_computer_use.pdf",
               "COL_internet_connection.pdf",
               "COL_internet_usage.pdf",
               "COL_internet_use_device.pdf",
               "COL_internet_use_purpose.pdf",
               "COL_no_internet_reason.pdf",
               "COL_radio_use.pdf",
               "COL_tv_ownership.pdf",
               "COL_travel_surface_2015.pdf")

# Checking paths to make sure things are pointed in the correct directory and pathname is valid
file.path(imagepath, map_order)

# Results in a rather large pdf, need to find a way to compress this a bit. But good enough for now.
pdf_combine(file.path(imagepath, map_order), output = file.path(imagepath, "COL_ICT_indicators_mapped_2019_12.pdf"))


