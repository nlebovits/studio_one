library(tidyverse)
library(janitor)

setwd("C:/Users/Nissim/Desktop/Spring 2023/Studio")

#--------------------------------PLOT SETUP--------------------------------------------------------#

# define color palette
woodland_palette_dark <- c(
  "#231f20",
           "#1c3c4a",
           "#344a2f",
           "#886ea9",
           "#e02c2d",
           "#ffb400"
)

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
woodland_palette_industry = c(
  "#231f20",
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

theme_woodland <- function() {
  theme_classic() %+replace% # replace elements we want to change
    
    theme(
      
      # grid elements
      panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dashed', lineend = "round"),
      axis.line = element_line(linewidth = 0.5, lineend = "round"),
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
        angle = 45
      ), # margin for axis text
      
      axis.text.y = element_text(
        hjust = -0.5
      ),
      aspect.ratio = 1 # make plot square
    )
}


#--------------------------------IMPORT--------------------------------------------------------#
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
  mutate(pct = as.(str_remove_all(pct, '%')))

income$income_and_benefits_2021 <- factor(income$income_and_benefits_2021, levels = c((unique(income$income_and_benefits_2021))))



income_plot = ggplot(income, aes(x = income_and_benefits_2021, y = pct, fill = geom)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Household Income",
    fill = "Area",
    y = '% of Population'
    
  ) +
  scale_fill_manual(values = c(
    "#344a2f",
             "#ffb400"
  )) +
  theme_woodland() +
  theme(
    axis.title.y = element_blank(),
    aspect.ratio = 1,
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.5, linetype = 'dashed', lineend = "round"),
    axis.text.x = element_text(angle = 30)
  )








industry <- ehs_tabs[16:29,] %>%
  pivot_longer(c("woodland", "philadelphia"), names_to = "geom", values_to = "pct") |>
  clean_names() |>
  filter(income_and_benefits_2021 != 'INDUSTRY - 2021') |>
  rename(industry = income_and_benefits_2021) |>
  mutate(pct = as.numeric(str_remove_all(pct, '%')))



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


#------------Export Data-----------------#

# this chunk creates a variable for today's date and formats it properly for the file name
today <- lubridate::ymd(Sys.Date()) |>
  str_replace_all("-", "_")

# vectorize plots, concatenate names, loop through ggsave
plots <- list(income_plot, industry_plot)

plot_names <- paste0(today, c("_income_plot.pdf", "_industry_plot.pdf"))

# this line saves the plot as a pdf with the proper formatting based on today's date
purrr::map2(plots, plot_names, ~ ggsave(.y, .x, path = "C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one", device = "pdf", bg = "transparent"))
