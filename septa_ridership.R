library(tidyverse)
library(sf)
library(tmap)
library(janitor)

setwd("C:/Users/Nissim/Desktop/Spring 2023/Studio")

# define color palette
woodland_palette_dark = c('#231f20',
                                   '#1c3c4a',
                                   '#344a2f',
                                   '#886ea9',
                                   '#e02c2d',
                                   '#ffb400'
)

#----------------------------------------------------------IMPORT DATA---------------------------------------------------------#

septa <- st_read('./SEPTA Ridership/Trolley_Ridership_Data.shp') |>
  st_transform(crs = st_crs('EPSG:2272')) |>
  clean_names()

woodland_septa = septa |>
  filter(route %in% c(11, 13, 36))

tmap_mode('view')

woodland_septa_map <- tm_shape(woodland_septa) +
  tm_sf(size = 'x2022_total',
        col = 'route',
        palette = woodland_palette_dark)


# this chunk creates a variable for today's date and formats it properly for the file name
today = lubridate::ymd(Sys.Date()) |>
  str_replace_all("-", '_')


tmap_save(woodland_septa_map, filename = paste0(today, '_woodland_septa_map.pdf'), width = 4.25, height = 4.25, units = 'in', dpi = 300)