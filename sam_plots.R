library(tidyverse)
library(janitor)
library(monochromeR)

setwd("C:/Users/Nissim/Desktop/Spring 2023/Studio")

#----------------------------------------------------------Define Plots---------------------------------------------------------#


## set plot options

# define color palette
grey_pal = generate_palette('darkgrey', 'go_lighter', n_colours = 8, view_palette = T)


race_pal = c(c('#1c3c4a', '#ffb400', '#344a2f'), 
             grey_pal[seq(2, length(grey_pal), by = 2)]) # prints every second element in the grey_pal list in order to emphasize color differences




theme_woodland <- function() {
  theme_classic() %+replace% # replace elements we want to change
    
    theme(
      
      # grid elements
      axis.line.x = element_line(linewidth = 1, lineend = 'round'), 
      axis.line.y = element_blank(), 
      axis.ticks = element_blank(), # strip axis ticks
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.5, lineend = "round",
                                        color = 'darkgrey'),
      
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
      
      axis.title.y = element_text(angle = 90),
      
      axis.text = element_text( # axis text
        size = 12
      ), # font size
      
      axis.text.x = element_text(
        hjust = 0.5, # center text
      ), # margin for axis text
      
      axis.text.y = element_text(
        hjust = -0.5,
        vjust = -0.5
      ),
      aspect.ratio = 1 # make plot square
    )
}


#----------------------------------------------------------Define Plots---------------------------------------------------------#

pop_race <- read.csv('./Existing_Conditions_Population_1970_2020.csv') |>
  filter(X != 'Statistics')

pop_race = t(pop_race) |>
  row_to_names(row_number = 1) |>
  as.data.frame() |>
  clean_names()

pop_race$year = c(1970, 1980, 1990, 2000, 2010, 2020)

rownames(pop_race) <- NULL

pop_race[pop_race == ''] <- 0

pop_race = map_df(pop_race, as.numeric) 

pop_race <- pop_race |>
  select(-total_population) |>
  pivot_longer(c('white', 
                        'black', 
                        'some_other_race', 
                        'american_indian_and_alaska_native_alone', 
                        'asian', 
                        'hispanic',
                        'native_hawaiian_and_other_pacific_islander_alone',
                        'multi_race'),
               names_to = 'race',
               values_to = 'count') |>
  mutate(race = case_when(
    race == 'white' ~ 'White',
    race == 'black' ~ 'Black',
    race ==  'some_other_race' ~ 'Other',
    race ==  'american_indian_and_alaska_native_alone' ~ 'Other',
    race ==  'asian' ~ 'Asian',
    race ==  'native_hawaiian_and_other_pacific_islander_alone' ~ 'Other',
    race == 'hispanic' ~ 'Hispanic',
    TRUE ~ "Other"
  ))


pop_race[pop_race$race == "Black" & pop_race$year == 2020, 'count'] <- 5622


race_plot = ggplot(pop_race, aes(x = year, y = count)) +
  geom_col(aes(fill = reorder(race, count))) +
  labs(
    title = "Woodland Avenue Pop. by Race",
    subtitle = 'Total Population, 1970 to 2020',
    fill = "Race",
  ) +
  scale_fill_manual(values = c( "Asian" = "#344a2f",
                                "Black" = "#ffb400",
                                "Other" = "#D0D0D0",
                                "White" = "#1c3c4a",
                                'Hispanic' = "#886ea9"
                                  
  )) +
  theme_woodland() +
  theme(
    axis.title = element_blank(),
  )



#------------Export Data-----------------#

# this chunk creates a variable for today's date and formats it properly for the file name
today = lubridate::ymd(Sys.Date()) |>
  str_replace_all("-", '_')

# vectorize plots, concatenate names, loop through ggsave
plots = list(race_plot)

plot_names = paste0(today, c('_race_plot.pdf'))

# this line saves the plot as a pdf with the proper formatting based on today's date
purrr::map2(plots, plot_names, ~ ggsave(.y, .x, path = 'C:/Users/Nissim/Desktop/Spring 2023/Studio/other_data_viz', device = 'pdf', bg = 'transparent'))