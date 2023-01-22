library(tidyverse)
library(sf)
library(tigris)
library(rphl)
library(plotly)
library(ggthemr)
library(tmap)

setwd('C:/Users/Nissim/Desktop/Spring 2023/Studio/gun_crime')

options(tigris_use_cache = TRUE) # turn on tigris caching

ggthemr("pale") #set global ggplot theme

tmap_mode("view")


## import woodland ave tracts

# import phl tracts; pull woodland ave tracts only
woodland_tracts = tracts('PA', 'Philadelphia') |>
                    filter(NAME %in% c(66, 63)) |>
                    st_transform(crs = st_crs("EPSG:2272")) # project to PA South NAD 1983 US Ft

#tm_shape(woodland_tracts) +
 # tm_polygons()


## query city's crime data

# define url for city's carto database
base_url = "https://phl.carto.com/api/v2/sql"

# define variable for six years ago to filter crime down
six_years_ago = (lubridate::ymd(Sys.Date()) - lubridate::years(6))

# define SQL query for database
query = sprintf("
        select dispatch_date_time, text_general_code, point_x, point_y
        from incidents_part1_part2
        where dispatch_date_time  >= '%s'
        ", six_years_ago)

# query crimes
crimes = st_as_sf(get_carto(query,
                            format = 'csv',
                            base_url = base_url,
                            stringsAsFactors = FALSE) |>
                  filter(!is.na(point_x),
                         !is.na(point_y)),
                        coords = c("point_x", "point_y"),
                        crs = st_crs('EPSG:4326')) |>
                  mutate(year = lubridate::year(dispatch_date_time)) |>
                  filter( between (year, 2018, 2022) ) |>
                  st_transform(crs = st_crs("EPSG:2272")) # project to PA South NAD 1983 US Ft


# phl crime

phl_gun_crime = crimes |>
                  filter(text_general_code %in% c('Robbery Firearm', 'Aggravated Assault Firearm')) |>
                  mutate(type = 'Gun')

st_write(phl_gun_crime, 'phl_guncrime_2018thru22.shp')


phl_econ_crime = crimes |>
                  filter(grepl('Robbery|Burglary|Theft', text_general_code)) |>
                  mutate(type = 'Econ')

st_write(phl_econ_crime, 'phl_econcrime_2018thru22.shp')


phl_drug_crime = crimes |>
                    filter(grepl('Drug', text_general_code)) |>
                    mutate(type = 'Drug')

st_write(phl_drug_crime, 'phl_drugcrime_2018thru22.shp') 


# woodland ave specific crime

woodland_gun_crime = phl_gun_crime[woodland_tracts, ]

woodland_econ_crime = phl_econ_crime[woodland_tracts, ]

woodland_drug_crime = phl_drug_crime[woodland_tracts, ]




ggplot(woodland_gun_crime) +
  geom_histogram(aes(x = year)) +
  labs(title = 'Gun Crimes on Woodland Ave',
       subtitle = '2018 through 2022',
       y = 'Total Crimes',
       x = 'Year')

ggplot(woodland_econ_crime) +
  geom_histogram(aes(x = year))  +
  labs(title = 'Economic Crimes on Woodland Ave',
       subtitle = '2018 through 2022',
       y = 'Total Crimes',
       x = 'Year')

ggplot(woodland_drug_crime) +
  geom_histogram(aes(x = year)) +
  labs(title = 'Drug Crimes on Woodland Ave',
       subtitle = '2018 through 2022',
       y = 'Total Crimes',
       x = 'Year')






all_woodland_crimes = rbind(woodland_drug_crime, woodland_gun_crime, woodland_econ_crime)

ggplot(all_woodland_crimes) +
  geom_histogram(aes(x = year, fill = type),
                 position = 'dodge',
                 binwidth = 0.75) +
  labs(title = 'Crimes on Woodland Ave',
       subtitle = '2018 through 2022',
       y = 'Total Crimes',
       x = 'Year',
       fill = "Type")
