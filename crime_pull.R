#------------Setup-----------------#



library(tidyverse)
library(sf)
library(tigris)
library(rphl)
library(tmap)
library(tidycensus)
library(acs)
library(showtext)

## set options

setwd('C:/Users/Nissim/Desktop/Spring 2023/Studio/gun_crime')

options(tigris_use_cache = TRUE) # turn on tigris caching

tmap_mode("view")


#------------Define Plots-----------------#


## set plot options

# define color palette
woodland_palette_full = c('#231f20',
                              '#1c3c4a',
                              '#344a2f',
                              '#886ea9',
                              '#e02c2d',
                              '#ffb400',
                              '#d2e6ee',
                              '#efefef',
                              '#e4eee6',
                              '#e8e8e8',
                              '#ecdace',
                              '#fff3d6',
                              '#9bc6d9',
                              '#c2c2c2',
                              '#a9aa7e',
                              '#d5cce1',
                              '#c79191',
                              '#ffe099'
)

# define color palette
woodland_palette_dark = c('#231f20',
                              '#1c3c4a',
                              '#344a2f',
                              '#886ea9',
                              '#e02c2d',
                              '#ffb400'
)

# define color palette
woodland_palette_light = c('#d2e6ee',
                              '#efefef',
                              '#e4eee6',
                              '#e8e8e8',
                              '#ecdace',
                              '#fff3d6',
                              '#9bc6d9',
                              '#c2c2c2',
                              '#a9aa7e',
                              '#d5cce1',
                              '#c79191',
                              '#ffe099'
                          )


## define plot theme

font_add_google(name = "Inter", family = "inter") # add google font

# turn on showtext
showtext_auto()

theme_woodland = function(){ 
#  font = "inter"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major.y = element_line(linetype = 'dashed'),    #strip major gridlines
      panel.grid.major.x = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
      #  family = font,            #set font family
        size = 20,                #set font size
        hjust = 0.5,                #left align
        vjust = 1),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
       # family = font,            #font family
        size = 14,                #font size
        vjust = -0.5),            # lower slightly   
      
      plot.caption = element_text(           #caption
   #     family = font,            #font family
        size = 11,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
   #     family = font,            #font family
        size = 12),               #font size
      
      axis.text = element_text(              #axis text
    #    family = font,            #axis famuly
        size = 12),                #font size
      
      axis.text.x = element_text( 
        hjust = 0.5, # center text 
        margin=margin(5, b = 11)), #margin for axis text
      
      axis.text.y = element_text(
        hjust = -0.5
      ),
      
      aspect.ratio = 1 #make plot square
      
    )
}



#------------Import Data-----------------#


## import woodland ave tracts

# import phl tracts; pull woodland ave tracts only
woodland_tracts = tracts('PA', 'Philadelphia') |>
                    filter(NAME %in% c(66, 63)) |>
                    st_transform(crs = st_crs("EPSG:2272")) # project to PA South NAD 1983 US Ft

tm_shape(woodland_tracts) +
  tm_polygons()


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



# pull population estimates
phl = get_acs(geography = "tract", # What is the lowest level of geography are we interested in?
              year = 2021, # What year do we want - this can also be used for 2000 census data
              variables = "B01003_001E", #Total population
              geometry = FALSE,
              state = "PA", # What state?
              county = "Philadelphia", # What County?
              output = "wide") 

woodland = phl |>
  filter(GEOID %in% c('42101006600', '42101006300'))

phl_pop =  sum(phl$B01003_001E)

woodland_pop = sum(woodland$B01003_001E)
#------------Clean Data-----------------#


### total phl crime

phl_gun_crime = crimes |>
                  filter(text_general_code %in% c('Robbery Firearm', 'Aggravated Assault Firearm')) |>
                  mutate(type = 'Gun')


phl_econ_crime = crimes |>
                  filter(grepl('Robbery|Burglary|Theft', text_general_code)) |>
                  mutate(type = 'Econ')


phl_drug_crime = crimes |>
                    filter(grepl('Drug', text_general_code)) |>
                    mutate(type = 'Drug')

 

### woodland ave specific crime

woodland_gun_crime = phl_gun_crime[woodland_tracts, ]

woodland_econ_crime = phl_econ_crime[woodland_tracts, ]

woodland_drug_crime = phl_drug_crime[woodland_tracts, ]


all_woodland_crimes = rbind(woodland_drug_crime, woodland_gun_crime, woodland_econ_crime)



### annual phl crime per capita
annual_phl_gun_crime = phl_gun_crime |>
  group_by(year) |>
  tally() |>
  rename(total_gun_crime = n) |>
  mutate(tot_pop = phl_pop,
         guncrime_per_cap = total_gun_crime / tot_pop,
         geom = 'Philadelphia')

annual_phl_econ_crime = phl_econ_crime |>
  group_by(year) |>
  tally() |>
  rename(total_econ_crime = n) |>
  mutate(tot_pop = phl_pop,
         econcrime_per_cap = total_econ_crime / tot_pop,
         geom = 'Philadelphia')

annual_phl_drug_crime = phl_drug_crime |>
  group_by(year) |>
  tally() |>
  rename(total_drug_crime = n) |>
  mutate(tot_pop = phl_pop,
         drugcrime_per_cap = total_drug_crime / tot_pop,
         geom = 'Philadelphia')



### annual woodland ave crime per capita
annual_woodland_gun_crime = woodland_gun_crime |>
  group_by(year) |>
  tally() |>
  rename(total_gun_crime = n) |>
  mutate(tot_pop = woodland_pop,
         guncrime_per_cap = total_gun_crime / tot_pop,
         geom = 'Woodland')

annual_woodland_econ_crime = woodland_econ_crime |>
  group_by(year) |>
  tally() |>
  rename(total_econ_crime = n) |>
  mutate(tot_pop = woodland_pop,
         econcrime_per_cap = total_econ_crime / tot_pop,
         geom = 'Woodland')

annual_woodland_drug_crime = woodland_drug_crime |>
  group_by(year) |>
  tally() |>
  rename(total_drug_crime = n) |>
  mutate(tot_pop = woodland_pop,
         drugcrime_per_cap = total_drug_crime / tot_pop,
         geom = 'Woodland')


# combine woodland level and phl level crime for comparison
tot_gun_crime = rbind(annual_woodland_gun_crime, annual_phl_gun_crime)
  
tot_econ_crime = rbind(annual_woodland_econ_crime, annual_phl_econ_crime)

tot_drug_crime = rbind(annual_woodland_drug_crime, annual_phl_drug_crime)

#------------Graph Data-----------------#


windows()

all_crime_plot = ggplot(all_woodland_crimes) +
                    geom_histogram(aes(x = year, fill = type), alpha = 0.7, binwidth = 0.5) +
                    labs(title = 'Crimes on Woodland Ave',
                         subtitle = '2018 through 2022',
                         fill = "Type") +
                    scale_fill_manual(values = rev(woodland_palette_light)) +
                    theme_woodland() +
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank())


## annual gun crimes per capita
gun_crime_per_cap_plot = ggplot(tot_gun_crime) +
                            geom_col(aes(x = year, y = guncrime_per_cap, fill = geom),  alpha = 0.7, position = 'dodge') +
                            labs(title = 'Gun Crimes on Woodland Ave',
                                 subtitle = '2018 through 2022',
                                 y = 'Crimes per Capita',
                                 x = 'Year',
                                 fill = "Geography") +
                            scale_fill_manual(values = rev(woodland_palette_light)) +
                            theme_woodland()

## annual gun crimes per capita
econ_crime_per_cap_plot = ggplot(tot_econ_crime) +
                            geom_col(aes(x = year, y = econcrime_per_cap, fill = geom),  alpha = 0.7, position = 'dodge') +
                            labs(title = 'Econ Crimes on Woodland Ave',
                                 subtitle = '2018 through 2022',
                                 y = 'Crimes per Capita',
                                 x = 'Year',
                                 fill = "Geography") +
                            scale_fill_manual(values = rev(woodland_palette_light)) +
                            theme_woodland()

## annual drug crimes per capita
drug_crime_per_cap_plot = ggplot(tot_drug_crime) +
                            geom_col(aes(x = year, y = drugcrime_per_cap, fill = geom),  alpha = 0.7, position = 'dodge') +
                            labs(title = 'Drug Crimes on Woodland Ave',
                                 subtitle = '2018 through 2022',
                                 y = 'Crimes per Capita',
                                 x = 'Year',
                                 fill = "Geography") +
                            scale_fill_manual(values = rev(woodland_palette_light)) +
                            theme_woodland()


#------------Export Data-----------------#

# this chunk creates a variable for today's date and formats it properly for the file name
today = lubridate::ymd(Sys.Date()) |>
                str_replace_all("-", '_')


# this line saves the plot as a pdf with the proper formatting based on today's date
ggsave(paste0(today, '_all_crime_plot.pdf'), all_crime_plot, path = 'C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one', device = 'pdf', bg = 'transparent')

# this line saves the plot as a pdf with the proper formatting based on today's date
ggsave(paste0(today, '_gun_crime_per_cap_plot.pdf'), gun_crime_per_cap_plot, path = 'C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one', device = 'pdf', bg = 'transparent')

# this line saves the plot as a pdf with the proper formatting based on today's date
ggsave(paste0(today, '_econ_crime_per_cap_plot.pdf'), econ_crime_per_cap_plot, path = 'C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one', device = 'pdf', bg = 'transparent')

# this line saves the plot as a pdf with the proper formatting based on today's date
ggsave(paste0(today, '_drug_crime_per_cap_plot.pdf'), drug_crime_per_cap_plot, path = 'C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one', device = 'pdf', bg = 'transparent')



st_write(phl_gun_crime, paste0(today, '_phl_guncrime_2018thru22.shp'))

st_write(phl_econ_crime, paste0(today,'_phl_econcrime_2018thru22.shp'))

st_write(phl_drug_crime, paste0(today,'_phl_drugcrime_2018thru22.shp'))
