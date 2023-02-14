# date: 2023_02_13
# purpose: full plots for the woodland ave book

# contents (alphabetical):
  # outline palettes
  # outline ggplot theme
  # delfina's plots
  # ed's plots
  # laura's plots 
  # sam's plots
  # print all

#--------------------------------------------SETUP--------------------------------------------# 
library(tidyverse)
library(janitor)
library(monochromeR)

setwd('C:/Users/Nissim/Desktop/Spring 2023/Studio')

today <- lubridate::ymd(Sys.Date()) |>
  str_replace_all("-", "_")


#--------------------------------------------OUTLINE PALETTES--------------------------------------------# 

woodland_palette_dark <- c(
                            "#231f20",
                            "#1c3c4a",
                            "#344a2f",
                            "#886ea9",
                            "#e02c2d",
                            "#ffb400"
                          )


woodland_palette_full <- c(
                            "#231f20",
                            "#1c3c4a",
                            "#344a2f",
                            "#886ea9",
                            "#e02c2d",
                            "#ffb400",
                            "#d2e6ee",
                            "#efefef",
                            "#e4eee6",
                            "#e8e8e8",
                            "#ecdace",
                            "#fff3d6",
                            "#9bc6d9",
                            "#c2c2c2",
                            "#a9aa7e",
                            "#d5cce1",
                            "#c79191",
                            "#ffe099"
                          )


woodland_palette_industry <- c(
                                "#231f20",
                                "#d2e6ee",
                                "#efefef",
                                "#e4eee6",
                                "#e8e8e8",
                                "#ecdace",
                                "#fff3d6",
                                "#9bc6d9",
                                "#c2c2c2",
                                "#a9aa7e",
                                "#d5cce1",
                                "#c79191",
                                "#ffe099"
                              )


#--------------------------------------------GGPLOT THEME--------------------------------------------# 
theme_woodland <- function() {
                              theme_classic() %+replace% # replace elements we want to change
                            
                                theme(
                            
                                  # grid elements
                                  panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'solid', lineend = "round"),
                                  axis.line.x = element_line(linewidth = 0.5, lineend = "round"),
                                  axis.line.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                            
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
                                    angle = 45
                                  ), # margin for axis text
                            
                                  axis.text.y = element_text(
                                    hjust = -0.33,
                                    vjust = -1
                                  ),
                                  aspect.ratio = 1 # make plot square
                                )
                            }


#--------------------------------------------DELFINA'S PLOTS--------------------------------------------# 


#--------------------------------------------ED'S PLOTS--------------------------------------------# 

### IMPORT

ehs_tabs <- read.csv('./EHS_Tables_For_Naseem.csv') |>
  dplyr::select(-c(X, X.1)) |>
  clean_names() |>
  rename(woodland = percent_woodland,
         philadelphia = percent_philadelphia)



med_hh_inc <- ehs_tabs[12,]



income <- ehs_tabs[1:11,] %>%
  pivot_longer(c("woodland", "philadelphia"), names_to = "geom", values_to = "pct") |>
  clean_names() |>
  filter(income_and_benefits_2021 != 'Total households') |>
  mutate(pct = as.numeric(str_remove_all(pct, '%')))

income$income_and_benefits_2021 <- factor(income$income_and_benefits_2021, levels = c((unique(income$income_and_benefits_2021))))


industry <- ehs_tabs[16:29,] %>%
  pivot_longer(c("woodland", "philadelphia"), names_to = "geom", values_to = "pct") |>
  clean_names() |>
  filter(income_and_benefits_2021 != 'INDUSTRY - 2021') |>
  rename(industry = income_and_benefits_2021) |>
  mutate(pct = as.numeric(str_remove_all(pct, '%')))


### PLOT


income_plot = ggplot(income, aes(x = income_and_benefits_2021, y = pct, fill = geom)) +
                    geom_col(position = "dodge") +
                    coord_flip() +
                    labs(
                      title = "2021 Median Household Income",
                      y = '% of Population'
                      
                    ) +
                    scale_fill_manual(values = c(
                      "#a9aa7e",
                               "#ffb400"
                    )) +
                    theme_woodland() +
                    theme(
                      panel.grid.major.x = element_line(linewidth = 0.5, linetype = 'solid', lineend = "round"),
                      panel.grid.major.y = element_blank(),
                      axis.line = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.x = element_text(angle = 0),
                      legend.title = element_blank(),
                      legend.position = 'bottom'
                    )








industry_plot = ggplot(industry, aes(x = geom, y = pct, fill = industry)) +
  coord_flip() + 
  geom_col(position = "dodge") +
  labs(
    title = "Industry Share",
    fill = "Industry",
  ) +
  scale_fill_manual(values = woodland_palette_industry
  ) +
  theme_woodland() +
  theme(
    axis.title.x = element_blank(),
    aspect.ratio = 1,
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dashed', lineend = "round"),
    axis.text.x = element_text(angle = 0)
  )


### name

# vectorize plots, concatenate names, loop through ggsave
ed_plots <- list(income_plot, industry_plot)

ed_plot_names <- paste0(today, c("_income_plot.pdf", "_industry_plot.pdf"))




#--------------------------------------------LAURA'S PLOTS--------------------------------------------# 

### CREATE DATA


# median gross rent

mgr = as.data.frame(rbind(
  c(975, 'Woodland Avenue', 2011),
  c(1026, 'Philadelphia', 2011),
  c(962, 'Woodland Avenue', 2021),
  c(1149, 'Philadelphia', 2021)
))

colnames(mgr) <- c('median_gross_rent', 'geom', 'year')

mgr$median_gross_rent <- as.numeric(mgr$median_gross_rent)




# vacancy rate

vacancy = as.data.frame(rbind(
  c(19.4, 'Woodland Avenue', 2011),
  c(13.7, 'Philadelphia', 2011),
  c(20.1, 'Woodland Avenue', 2021),
  c(10.3, 'Philadelphia', 2021)
))

colnames(vacancy) <- c('vacancy_rt', 'geom', 'year')

vacancy$vacancy_rt <- as.numeric(vacancy$vacancy_rt)


# occupancy type

occupancy = as.data.frame(rbind(
  c(35.0, 'Woodland Avenue', 'Owner', 2011),
  c(54.9, 'Philadelphia', 'Owner', 2011),
  c(65.0, 'Woodland Avenue', 'Renter', 2011),
  c(45.1, 'Philadelphia', 'Renter', 2011),
  c(32.5, 'Woodland Avenue', 'Owner', 2021),
  c(52.4, 'Philadelphia', 'Owner', 2021),
  c(67.5, 'Woodland Avenue', 'Renter', 2021),
  c(47.6, 'Philadelphia', 'Renter', 2021)
  
))

colnames(occupancy) <- c('pct', 'geom', 'occupancy', 'year')

occupancy$pct <- as.numeric(occupancy$pct)



# median property value
med_prop_val = as.data.frame(rbind(
  c(100065, 'Woodland Avenue', 2011),
  c(169862, 'Philadelphia', 2011),
  c(67309, 'Woodland Avenue', 2021),
  c(184100, 'Philadelphia', 2021)
))

colnames(med_prop_val) <- c('med_val', 'geom', 'year')

med_prop_val$med_val <- as.numeric(med_prop_val$med_val)



# monthly housing costs

housing_costs = as.data.frame(rbind(
  c('Less than $20,000', 532, 'Woodland Avenue', 'Total'),
  c('Less than $20,000', 78542, 'Philadelphia', 'Total'),
  c('Less than $20,000', 464, 'Woodland Avenue', 'Rent-Burdened'),
  c('Less than $20,000', 70614, 'Philadelphia', 'Rent-Burdened'),
  c('$20,000 to $34,999', 509, 'Woodland Avenue', 'Total'),
  c('$20,000 to $34,999', 45012, 'Philadelphia', 'Total'),
  c('$20,000 to $34,999', 286, 'Woodland Avenue', 'Rent-Burdened'),
  c('$20,000 to $34,999', 37517, 'Philadelphia', 'Rent-Burdened'),
  c('$35,000 to $49,999', 287, 'Woodland Avenue', 'Total'),
  c('$35,000 to $49,999', 38836, 'Philadelphia', 'Total'),
  c('$35,000 to $49,999', 116, 'Woodland Avenue', 'Rent-Burdened'),
  c('$35,000 to $49,999', 23025, 'Philadelphia', 'Rent-Burdened'),
  c('$50,000 to $74,999', 193, 'Woodland Avenue', 'Total'),
  c('$50,000 to $74,999', 49542, 'Philadelphia', 'Total'),
  c('$50,000 to $74,999', 0, 'Woodland Avenue', 'Rent-Burdened'),
  c('$50,000 to $74,999', 14050, 'Philadelphia', 'Rent-Burdened'),
  c('$75,000 or More', 242, 'Woodland Avenue', 'Total'),
  c('$75,000 or More', 74419, 'Philadelphia', 'Total'),
  c('$75,000 or More', 0, 'Woodland Avenue', 'Rent-Burdened'),
  c('$75,000 or More', 3500, 'Philadelphia', 'Rent-Burdened')
))


colnames(housing_costs) <- c('income_lvl', 'count', 'geom', 'category')

housing_costs$count <- as.numeric(housing_costs$count)


rent_burd <- housing_costs |>
  filter(category == 'Rent-Burdened') |>
  rename(rent_burd_pop = count)

tot = housing_costs |>
  filter(category == 'Total') |>
  select(count) |>
  rename(tot_pop = count)

not_rent_burd = cbind(rent_burd, tot) |>
  mutate(no_burd = tot_pop - rent_burd_pop,
         category = 'Not Rent-Burdened') |>
  select(-c(rent_burd_pop, tot_pop)) |>
  rename(count = no_burd)

housing_costs = rbind(housing_costs, not_rent_burd)

housing_costs$income_lvl <- factor(housing_costs$income_lvl, levels = c((unique(housing_costs$income_lvl))))



households_pct <- as.data.frame(rbind(
  c("With Children", "Males", 0),
  c("Single Householder", "Males", 64),
  c("65+ years old", "Males", 16),
  c("With Children", "Females", 34),
  c("Single Householder", "Females", 45),
  c("65+ years old", "Females", 12)
))

colnames(households_pct) <- c("type", 'gender', 'pct')

households_pct$pct <- as.numeric(households_pct$pct)



households_count <- as.data.frame(rbind(
  c("Count", "Males", 822),
  c("Count", "Females", 1527)
))

colnames(households_count) <- c("type", 'gender', 'count')

households_count$count <- as.numeric(households_count$count)


### PLOT

mgr_plot = ggplot(mgr, aes(x = year, y = median_gross_rent, fill = geom)) +
                geom_col(position = "dodge") +
                labs(
                  title = "Median Gross Rent",
                  subtitle = '2011 to 2021',
                  fill = "Geography",
                  y = 'Median Rent ($)'
                  
                ) +
                #  scale_y_continuous(limits = c(400, 1200)) +
                scale_fill_manual(values = c(
                  "#a9aa7e",
                           "#ffb400"
                )) +
                theme_woodland() +
                theme(
                  axis.title.x = element_blank(),
                  aspect.ratio = 1,
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dashed', lineend = "round"),
                  axis.text.x = element_text(angle = 30)
                )



vacancy_plot = ggplot(vacancy, aes(x = year, y = vacancy_rt, fill = geom)) +
                                  geom_col(position = "dodge") +
                                  geom_text(aes(label = paste0(vacancy_rt, "%"), y = vacancy_rt + 0.5), size = 4, color = "black", position = position_dodge(width = 0.9)) +
                                  labs(
                                    title = "Residential Vacancy Rate",
                                    subtitle = '2011 to 2021',
                                    fill = "Geography",
                                    y = 'Vacancy Rate (%)'
                                    
                                  ) +
                                  #  scale_y_continuous(limits = c(400, 1200)) +
                                  scale_fill_manual(values = c(
                                    "#a9aa7e",
                                             "#ffb400"
                                  )) +
                                  theme_woodland() +
                                  theme(axis.title.x = element_blank(),
                                        axis.text.x = element_text(angle = 30))


occupancy_plot = ggplot(occupancy, aes(x = year, y = pct, fill = occupancy)) +
                                  geom_col(position = "stack") +
                                  facet_wrap(~geom) +
                                  labs(
                                    title = "Occupancy Type",
                                    subtitle = '2011 to 2021',
                                    fill = "Occupancy Type",
                                    y = 'Occupancy Share (5))'
                                    
                                  ) +
                                  scale_fill_manual(values = c(
                                    "#a9aa7e",
                                             "#ffb400"
                                  )) +
                                  theme_woodland() +
                                  theme(
                                    axis.title.x = element_blank(),
                                    aspect.ratio = 1,
                                    panel.grid.major.x = element_blank(),
                                    panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dashed', lineend = "round"),
                                    axis.text.x = element_text(angle = 30)
                                  )

prop_val_plot = ggplot(med_prop_val, aes(x = year, y = med_val, fill = geom)) +
                    geom_col(position = "dodge") +
                    geom_text(aes(label = paste0("$", formatC(med_val, format = 'd', big.mark = ",")), y = med_val + 5000), size = 4, color = "black", position = position_dodge(width = 0.9)) +
                    labs(
                      title = "Adjusted Median Property Value",
                      fill = "Geography",
                      y = 'Median Value ($)'
                      
                    ) +
                    #  scale_y_continuous(limits = c(400, 1200)) +
                    scale_fill_manual(values = c(
                      "#a9aa7e",
                               "#ffb400"
                    )) +
                    theme_woodland() +
                    theme(
                      axis.title.x = element_blank(),
                      axis.text.x = element_text(angle = 30)
                    )


rent_burd_plot = ggplot((housing_costs |> filter(category != 'Total', geom == 'Woodland Avenue')), 
                        aes(x = income_lvl, y = count, fill = category)) +
                        geom_col(position = "stack") +
                        labs(
                          title = "Local Rent Burden by Income",
                          fill = "Rent Burdened?",
                          y = 'Number of Households'
                          
                        ) +
                        scale_fill_manual(values = c(
                          "#fff3d6",
                                   "#ffb400"
                        )) +
                        theme_woodland() +
                        theme(
                          axis.title.x = element_blank(),
                          axis.text.x = element_text(angle = 30)
                        )


hhs_pct_plot = ggplot(households_pct, aes(x = type, y = ifelse(gender == "Males", pct, -pct), fill=gender))+  
                      geom_bar(stat="identity", position="identity") +
                      labs(title = "Household Types by Gender",
                           y = "Percent") +
                      scale_y_continuous(limits = c(-max(households_pct$pct), max(households_pct$pct))) +
                      scale_fill_manual(values = c(
                        '#d5cce1',
                                 '#9bc6d9'
                      )) +
                      theme_woodland() +
                      theme(axis.title.y = element_blank()) +
                      coord_flip()


hhs_count_plot = ggplot(households_count, aes(x = type, y = ifelse(gender == "Males", count, -count), fill=gender))+  
                      geom_bar(stat="identity", position="identity") +
                      labs(title = "Household Types by Gender",
                           y = "Total Households") +
                      scale_y_continuous(limits = c(-max(households_count$count), max(households_count$count))) +
                      scale_fill_manual(values = c(
                        '#d5cce1',
                                 '#9bc6d9'
                      )) +
                      theme_woodland() +
                      theme(axis.title.y = element_blank()) +
                      coord_flip()


### VECTORIZE PLOTS AND FILE NAMES

laura_plots <- list(mgr_plot, vacancy_plot, occupancy_plot, prop_val_plot, rent_burd_plot, hhs_pct_plot, hhs_count_plot)

laura_plot_names <- paste0(today, c("_mgr_plot.pdf", "_vacancy_plot.pdf", "_occupancy_plot.pdf", "_prop_val_plot.pdf", "_rent_burd_plot.pdf", "_hhs_pct_plot.pdf", "_hhs_count_plot.pdf"))

#--------------------------------------------SAM'S PLOTS--------------------------------------------# 

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
  )) %>%
  group_by(year, race) %>%
  summarise(count = sum(count)) %>%
  ungroup()


pop_race[pop_race$race == "Black" & pop_race$year == 2020, 'count'] <- 5622



race_plot = ggplot(pop_race, aes(x = year, y = count)) +
                    geom_line(aes(color = race), lwd = 1) +
                    geom_point(aes(color = race), size = 2) +
                    labs(
                      title = "Woodland Avenue Population by Race",
                      color = "Race",
                    ) +
                    scale_color_manual(values = c( "Asian" = "#886ea9",
                                                  "Black" =  "#1c3c4a",
                                                  "Other" = "#D0D0D0",
                                                  "White" = "#ffb400",
                                                  'Hispanic' = "#e02c2d"
                                                    
                    )) +
                    theme_woodland() +
                    theme(
                      axis.title = element_blank(),
                    )

# vectorize plots, concatenate names, loop through ggsave
sam_plots = list(race_plot)

sam_plot_names = paste0(today, c('_race_plot.pdf'))

purrr::map2(sam_plots, sam_plot_names, ~ ggsave(.y, .x, path = "C:/Users/Nissim/Desktop/Spring 2023/Studio/other_data_viz", device = "pdf", bg = "transparent"))

#--------------------------------------------PRINT ALL--------------------------------------------# 



plots <- c(ed_plots, laura_plots, sam_plots)

plot_names <- c(ed_plot_names, laura_plot_names, sam_plot_names)

purrr::map2(plots, plot_names, ~ ggsave(.y, .x, path = "C:/Users/Nissim/Desktop/Spring 2023/Studio/other_data_viz", device = "pdf", bg = "transparent"))