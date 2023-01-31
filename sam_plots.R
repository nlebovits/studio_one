library(tidyverse)
library(janitor)

setwd("C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one")

#----------------------------------------------------------Define Plots---------------------------------------------------------#


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



theme_woodland <- function() {
  theme_classic() %+replace% # replace elements we want to change
    
    theme(
      
      # grid elements
      axis.line = element_line(linewidth = 1, lineend = 'round'), 
      axis.ticks = element_blank(), # strip axis ticks
      
      # since theme_minimal() already strips axis lines,
      # we don't need to do that again
      
      # text elements
      plot.title = element_text( # title
        size = 20, # set font size
        hjust = 0.5, # left align
        vjust = 1
      ), # raise slightly
      
      plot.subtitle = element_text( # subtitle
        size = 14, # font size
        vjust = -0.5
      ), # lower slightly
      
      plot.caption = element_text( # caption
        size = 11, # font size
        hjust = 1
      ), # right align
      
      axis.title = element_text( # axis titles
        size = 12
      ), # font size
      
      axis.text = element_text( # axis text
        size = 12
      ), # font size
      
      axis.text.x = element_text(
        hjust = 0.5, # center text
      ), # margin for axis text
      
      axis.text.y = element_text(
        hjust = -0.5
      ),
      aspect.ratio = 1 # make plot square
    )
}


#----------------------------------------------------------Define Plots---------------------------------------------------------#

pop_race <- read.csv('./Existing_Conditions_Population_1970_2010.csv') |>
              filter(X != 'Statistics')

pop_race = t(pop_race) |>
              row_to_names(row_number = 1) |>
              as.data.frame() |>
              clean_names()

pop_race$year = c(1970, 1980, 1990, 2000, 2010)

rownames(pop_race) <- NULL

pop_race[pop_race == ''] <- 0

pop_race = map_df(pop_race, as.numeric) 

pop_race <- pop_race |>
              select(-total_population) |>
              pivot_longer(c('white_alone', 
                             'black_or_african_american_alone', 
                             'some_other_race', 
                             'american_indian_and_alaska_native_alone', 
                             'asian_alone', 
                             'native_hawaiian_and_other_pacific_islander_alone',
                             'two_or_more_races'),
                           names_to = 'race',
                           values_to = 'count') |>
             mutate(race = case_when(
                                      race == 'white_alone' ~ 'White Alone',
                                      race == 'black_or_african_american_alone' ~ 'Black or African American Alone',
                                      race ==  'some_other_race' ~ 'Some Other Race',
                                      race ==  'american_indian_and_alaska_native_alone' ~ 'American Indian and Alaska Native Alone',
                                      race ==  'asian_alone' ~ 'Asian Alone',
                                      race ==  'native_hawaiian_and_other_pacific_islander_alone' ~ 'Native Hawaiian and Other Pacific Islander Alone',
                                      TRUE ~ "Two or More Races"
             ))


race_plot = ggplot(pop_race, aes(x = year, y = count)) +
                  geom_line(aes(color = race)) +
                  geom_point(aes(color = race)) +
                  labs(
                    title = "Woodland Avenue Pop. by Race",
                    subtitle = '1970 to 2010',
                    color = "Race",
                    y = 'Total Population'
                  ) +
                  scale_color_manual(values = c('#344a2f',
                                     '#886ea9',
                                     '#1c3c4a',
                                     '#e02c2d',
                                     '#ffb400',
                                     '#9bc6d9',
                                     '#a9aa7e',
                                     '#c79191',
                                     '#ffe099')) +
                  theme_woodland() +
                  theme(
                    axis.title.x = element_blank(),
                    aspect.ratio = 1,
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dashed', lineend = "round"),
                    axis.text.x = element_text(angle = 0)
                  )


#------------Export Data-----------------#

# this chunk creates a variable for today's date and formats it properly for the file name
today = lubridate::ymd(Sys.Date()) |>
  str_replace_all("-", '_')

# vectorize plots, concatenate names, loop through ggsave
plots = list(race_plot)

plot_names = paste0(today, c('_race_plot.pdf'))

# this line saves the plot as a pdf with the proper formatting based on today's date
purrr::map2(plots, plot_names, ~ ggsave(.y, .x, path = 'C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one', device = 'pdf', bg = 'transparent'))