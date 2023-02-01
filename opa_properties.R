library(tidyverse)
library(sf)
library(rphl)

setwd("C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one")

# downloaded from https://www.opendataphilly.org/dataset/opa-property-assessments

opa_properties <- st_read('./opa_properties_public.geojson')%>%
  st_transform(crs = st_crs('EPSG:2272')) |>
  select(census_tract,
         exterior_condition,
         frontage,
         garage_spaces,
         garage_type,
         house_number,
         interior_condition,
         location,
         market_value,
         market_value_date,
         quality_grade,
         street_name,
         total_area,
         year_built,
         total_livable_area,
         unfinished,
         unit)


# this chunk creates a variable for today's date and formats it properly for the file name
today <- lubridate::ymd(Sys.Date()) |>
  str_replace_all("-", "_")

st_write(opa_properties, paste0(today, '_opa_properties.shp'))